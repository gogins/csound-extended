local ChordSpaceView = {}
function ChordSpaceView.help()
print [[

C H O R D S P A C E   V I E W
Copyright 2010 by Michael Gogins.

This software is licensed under the terms
of the GNU Lesser General Public License.

The purposes of this package are:

(1) To test the ChordSpace package for correctness.

(2) To demonstrate chord space for trichords.

TODO:

(1) Actually play chords using Fluidsynth.

(2) Alpha so OPTI is most solid, then in decreasing solidity, with OP visible
    and R just barely visible.

KEY BINDINGS

[Esc]                   Exit program.
[c]                     Iterate/stop iterating inversions.
[Keypad +]              Zoom in.
[Keypad -]              Zoom out.
[Keypad left arrow]     Move left.
[Keypad right arrow]    Move right.
[Keypad up arrow]       Move up.
[Keypad down arrow]     Move down.
[Keypad 9]              Spin in.
[Keypad 3]              Spin out.
[Keypad 7]              Spin left.
[Keypad 1]              Spin right.
[Keypad 8]              Spin up.
[Keypad 2]              Spin down.
[T]                     Transpose up.
[Shift][T]              Transpose down.
[I]                     Invert (reflect in the origin).
[P]                     Apply the neo-Riemannian parallel transformation.
[L]                     Apply the neo-Riemannian leading-tone exchange
                        transformation.
[R]                 `   Apply the neo-Riemannian relative minor/major
                        transformation.
[D]                     Apply the neo-Riemannian dominant transformation.
[K]                     Apply the neo-Riemannian K transformation
                        (change "modality").
[Q]                     Apply the neo-Riemannian Q transformation by 1 semitone
                        (contextual transposition depending on "modality").
[1]                     Move voice 1 by 1 semitone up.
[Shift][1]              Move voice 1 by 1 semitone down.
[2]                     Move voice 2 by 1 semitone up.
[Shift][2]              Move voice 2 by 1 semitone down.
[3]                     Move voice 3 by 1 semitone up.
[Shift][3]              Move voice 3 by 1 semitone down.
]]
end

local ffi  = require( "ffi" )
local glfw = require( "ffi/glfw" )
local gl   = require( "ffi/OpenGL" )
local glu  = require( "ffi/glu" )
local bit  = require( "bit" )

--local ffi =         require("ffi")
--local gl =          require("gl")
print('gl:', gl)
--local glu =         require("glu")
print('glu:', glu)
--local glfw =        require("glfw")
print('glfw:', glfw)
local Silencio =    require("Silencio")
--local ChordSpace =  require("Chords")
local ChordSpace =  require("ChordSpaces")

tx = 0
ty = 0
tz = 0

rx = 0
ry = 0
rz = 0

LightAmbient = ffi.new("float[4]")
LightAmbient[0] = .25
LightAmbient[1] = .25
LightAmbient[2] = .25
LightAmbient[3] = 1
LightDiffuse = ffi.new("float[4]")
LightDiffuse[0] = 1
LightDiffuse[1] = 1
LightDiffuse[2] = 1
LightDiffuse[3] = 1
LightPosition = ffi.new("float[4]")
LightPosition[0] = 100
LightPosition[1] = 100
LightPosition[2] = 1000
LightPosition[2] = 1
Ambient = ffi.new("float[4]")
Ambient[0] = 1
Ambient[1] = 1
Ambient[2] = 1
Ambient[3] = 1
Diffuse = ffi.new("float[4]")
Diffuse[0] = 1
Diffuse[1] = 1
Diffuse[2] = 1
Diffuse[3] = 1
Specular = ffi.new("float[4]")
Specular[0] = 1
Specular[1] = 1
Specular[2] = 1
Specular[3] = 1
Emission = ffi.new("float[4]")
Emission[0] = 0
Emission[1] = 0
Emission[2] = 0
Emission[3] = 1

-- Return the red, green, blue color corresponding to a hue, saturation, value color.

function hsv_to_rgb(h, s, v)
    local hi = math.floor(h / 60.0) % 6
    local f =  (h / 60.0) - math.floor(h / 60.0)
    local p = v * (1.0 - s)
    local q = v * (1.0 - (f * s))
    local t = v * (1.0 - ((1.0 - f) * s))
    if      hi == 0 then
        return v, t, p
    else if hi == 1 then
        return q, v, p
    else if hi == 2 then
        return p, v, t
    else if hi == 3 then
        return p, q, v
    else if hi == 4 then
        return t, p, v
    else if hi == 5 then
        return v, p, q
    end end end end end end
end

function iterateColor(c)
    r = c[1] + 1
    if r < 100 then
        c[1] = r
    else
        c[1] = 1
        g = c[2] + 1
        if g < 100 then
            c[2] = g
        else
            c[2] = 1
            c[3] = c[3] + 1
        end
    end
end

ChordView = {}

function ChordView:new(o)
    local o = o or {title = 'Chord View', octaves = 3, equivalence = 'OP', chords = {}, minima = {}, maximuma = {}, ranges = {}, fullscreen = true, minima = {}, maxima = {}, ranges = {}, pickedChord = nil}
    setmetatable(o, self)
    self.__index = self
    return o
end

function ChordView:material()
    gl.glShadeModel(gl.GL_SMOOTH)
    gl.glClearColor(0, 0, 0, 0.5)
    gl.glClearDepth(1.0)
    gl.glEnable(gl.GL_DEPTH_TEST)
    gl.glDepthFunc(gl.GL_LEQUAL)
    gl.glHint(gl.GL_PERSPECTIVE_CORRECTION_HINT, gl.GL_NICEST)
    gl.glEnable(gl.GL_COLOR_MATERIAL)
    gl.glLightfv(gl.GL_LIGHT1, gl.GL_AMBIENT, LightAmbient)
    gl.glLightfv(gl.GL_LIGHT1, gl.GL_DIFFUSE, LightDiffuse)
    gl.glLightfv(gl.GL_LIGHT1, gl.GL_POSITION, LightPosition)
    gl.glEnable(gl.GL_LIGHT1)
    gl.glMaterialfv(gl.GL_BACK, gl.GL_AMBIENT, Ambient)
    gl.glMaterialfv(gl.GL_BACK, gl.GL_DIFFUSE, Diffuse)
    gl.glMaterialfv(gl.GL_FRONT_AND_BACK, gl.GL_SPECULAR, Specular)
    gl.glMaterialfv(gl.GL_FRONT_AND_BACK, gl.GL_EMISSION, Emission)
    gl.glEnable(gl.GL_NORMALIZE)
    gl.glEnable(gl.GL_LIGHTING)
    gl.glEnable(gl.GL_BLEND)
    gl.glBlendFunc(gl.GL_SRC_ALPHA, gl.GL_ONE_MINUS_SRC_ALPHA)

end

function ChordView:draw(picking)
    picking = picking or false
    gl.glClear(gl.GL_COLOR_BUFFER_BIT)
    gl.glClear(gl.GL_DEPTH_BUFFER_BIT)
    self:material()
    gl.glLoadIdentity()
    gl.glTranslatef(tx,ty,tz)
    gl.glTranslatef(self.centerX, self.centerY, self.centerZ)
    gl.glRotatef(rx,1,0,0)
    gl.glRotatef(ry,0,1,0)
    gl.glRotatef(rz,0,0,1)
    gl.glTranslatef(-tx,-ty,-tz)
    gl.glTranslatef(-self.centerX, -self.centerY, -self.centerZ)
    gl.glTranslatef(tx,ty,tz)
    if not picking then
        self:drawGrid()
    end
    for name, chord in pairs(self.chords) do
        self:drawChord(chord, name, picking)
    end
    if not picking then
        glfw.glfwSwapBuffers(self.window)
    end
end

function ChordView:drawGrid()
    gl.glBegin(gl.GL_LINES)
    local range = ChordSpace.OCTAVE * self.octaves
    gl.glColor4f(1, 0, 0, 0.5)
    gl.glVertex3f(0, 0, 0)
    gl.glVertex3f(0, 0, range)
    gl.glVertex3f(0, 0, 0)
    gl.glVertex3f(0, range, 0)
    gl.glVertex3f(0, 0, 0)
    gl.glVertex3f(range, 0, 0)
    gl.glColor4f(0, 1, 0, 0.5)
    gl.glVertex3f(0, 0, 0)
    local orthogonalAxisPoints = math.sin(math.pi / 4) * range
    gl.glVertex3f(orthogonalAxisPoints, orthogonalAxisPoints, orthogonalAxisPoints)
    gl.glColor4f(1, 1, 1, 0.5)
    for i, c0 in pairs(self.chords) do
        if self:isE(c0) then
            c1 = c0:clone()
            c1[1] = c0[1] + 1
            if self:isE(c1) then
                gl.glVertex3f(c0[1], c0[2], c0[3])
                gl.glVertex3f(c1[1], c1[2], c1[3])
            end
            c2 = c0:clone()
            c2[2] = c0[2] + 1
            if self:isE(c2) then
                gl.glVertex3f(c0[1], c0[2], c0[3])
                gl.glVertex3f(c2[1], c2[2], c2[3])
            end
            c3 = c0:clone()
            c3[3] = c0[3] + 1
            if self:isE(c3) then
                gl.glVertex3f(c0[1], c0[2], c0[3])
                gl.glVertex3f(c3[1], c3[2], c3[3])
            end
        end
    end
    gl.glEnd()
end

function ChordView:resize(width, height)
    gl.glViewport(0, 0, width, height)
    local viewport = ffi.new('GLint[4]')
	gl.glGetIntegerv(gl.GL_VIEWPORT, viewport);
    gl.glMatrixMode(gl.GL_PROJECTION)
    gl.glLoadIdentity()
    glu.gluPerspective(45, (viewport[2] - viewport[0]) / (viewport[3] - viewport[1]), 0.1, 1000.0)
    gl.glMatrixMode(gl.GL_MODELVIEW)
    gl.glLoadIdentity()
    self.width_ = width
    self.height_ = height
    self.aspect = width / height
    print('aspect:', self.aspect)
    self.beginX = self.minima[1]
    self.beginY = self.minima[2]
    self.beginZ = self.minima[3]
    self.sizeX = self.ranges[1]
    self.sizeY = self.ranges[2]
    self.sizeZ = self.ranges[3]
    self.endX = self.maxima[1]
    self.endY = self.maxima[2]
    self.endZ = self.maxima[3]
    self.centerX = self.beginX + self.sizeX / 2
    self.centerY = self.beginY + self.sizeY / 2
    self.centerZ = self.beginZ + self.sizeZ / 2
    local boundingSize = self.sizeX
    if boundingSize < self.sizeY then
        boundingSize = self.sizeY
    end
    if boundingSize < self.sizeZ then
        boundingSize = self.sizeZ
    end
    self.left = self.centerX - boundingSize
    self.right = self.centerX + boundingSize
    self.top = self.centerY - boundingSize
    self.bottom = self.centerY + boundingSize
    if tonumber(self.aspect) < 1.0 then
        self.bottom =  self.bottom / self.aspect
        self.top = self.bottom / self.aspect
    else
        self.left = self.left * self.aspect
        self.right = self.right * self.aspect
    end
    self.front = self.centerZ + boundingSize * 2
    self.back = self.centerZ - boundingSize * 2
    print(string.format('Center: x: %9.4f, y: %9.4f, z: %9.4f', self.centerX, self.centerY, self.centerZ))
    print(string.format('Front: %9.4f, back: %9.4f', self.front, self.back))
    tx = -self.centerX
    ty = -self.centerY
    tz = -boundingSize * 3
    self.gridXs = {}
    self.gridYs = {}
    self.gridZs = {}
    local i = 1
    for x = self.beginX, self.endX, 10 do
        self.gridXs[i] = x
        i = i + 1
    end
    self.gridXs[i] = self.endX
    i = 1
    local modY = self.beginY % 12
    if modY ~= 0 then
        self.beginY = self.beginY - modY
    end
    for y = self.beginY, self.endY, 12 do
        self.gridYs[i] = y
        i = i + 1
    end
    self.gridYs[i] = self.endY
    i = 1
    for z = self.beginZ, self.endZ, 1 do
        self.gridZs[i] = z
        i = i + 1
    end
    self.gridZs[i] = self.endZ
          glu.gluLookAt(
	 0,  1, 0,   -- Eye-position
	 0, 20, 0,   -- View-point
	 0,  0, 1    -- Up Vector
      )
end

function ChordView:drawChord(chord, name, picking)
    picking = picking or false
    if picking then
        gl.glLoadName(name)
    end
    local alphafactor = 0.5
    gl.glPushMatrix()
    gl.glTranslatef(chord[1], chord[2], chord[3])
    --print(chord[1], chord[2], chord[3])
    gl.glBegin(gl.GL_QUADS)
    local quadric = glu.gluNewQuadric()
    glu.gluQuadricNormals(quadric, 100000) -- gl.GL_SMOOTH)
    local alpha = 1
    local radius = 0
    local red = 1
    local green = 1
    local blue = 1
    local z = chord:eOPT():et()
    if z      == self.augmentedTriad then
        red = 1
        green = 1
        blue = 1
    else if z == self.majorTriad1 then
        red = 1
        green = 0
        blue = 0
    else if z == self.minorTriad1 then
        red = 0
        green = 0
        blue = 1
    else
    if chord:iseT() == true then
        alpha = alpha * alphafactor
    end
    if chord:iseI() == true then
        blue = blue * alphafactor
    end
    end end end
    local saturation = 1.0
    local value = 1 - (0.375 * (chord:layer() / 12.0))
    --local value = 1
    red = red * value
    blue = blue * value
    green = green * value
    if chord:iseV(self.octaves * ChordSpace.OCTAVE) then
        alpha = alpha * alphafactor
    end
    --red, green, blue = hsv_to_rgb(hue, saturation, value)
    gl.glColor4f(red, green, blue, alpha)
    local radius = 1/6
    if self:isE(chord) then
        if self.pickedChord ~= nil then
            if self.pickedChord:information() == chord:information() then
                radius = radius * 1.75
                if self.drawInverse == true then
                    gl.glColor4f(1, 0, 1, 1)
                else
                    gl.glColor4f(1, 1, 1, 1)
                end
            end
        end
    end
    glu.gluSphere(quadric, radius, 20, 50)
    gl.glEnd()
    gl.glPopMatrix()
end

function ChordView:createChords()
    self.augmentedTriad = Chord:new{0, 4, 8}:eOPT():et()
    self.majorTriad1 =    Chord:new{0, 4, 7}:eOPT():et()
    self.minorTriad1 =    Chord:new{0, 3, 7}:eOPT():et()
    local dummy = nil
    if self.constructChordsByOperation then
        dummy, self.chords = ChordSpace.allOfEquivalenceClassByOperation(3, self.equivalence)
    else
        dummy, self.chords = ChordSpace.allOfEquivalenceClass(3, self.equivalence)
    end
    print(string.format('Created %s chords for equivalence class %s.', #self.chords, self.equivalence))
end

function ChordView:isE(chord)
    if self.equivalence == 'R' then
        return chord:iseR(self.octaves * ChordSpace.OCTAVE)
    end
    if self.equivalence == 'O' then
        return chord:iseO()
    end
    if self.equivalence == 'P' then
        return chord:iseP()
    end
    if self.equivalence == 'T' then
        return chord:iseT()
    end
    if self.equivalence == 'I' then
        return chord:iseI()
    end
    if self.equivalence == 'RP' then
        return chord:iseRP(self.octaves * ChordSpace.OCTAVE)
    end
    if self.equivalence == 'OP' then
        return chord:iseOP()
    end
    if self.equivalence == 'OT' then
        return chord:iseOT()
    end
    if self.equivalence == 'OI' then
        return chord:iseOI()
    end
    if self.equivalence == 'OPT' then
        return chord:iseOPT()
    end
    if self.equivalence == 'OPI' then
        return chord:iseOPI()
    end
    if self.equivalence == 'OPTI' then
        return chord:iseOPTI()
    end
end

function ChordView:E(chord)
    -- print('E: chord:', chord)
    if self.equivalence == 'R' then
        return chord:eR(self.octaves * ChordSpace.OCTAVE)
    end
    if self.equivalence == 'O' then
        return chord:eO()
    end
    if self.equivalence == 'P' then
        return chord:eP()
    end
    if self.equivalence == 'T' then
        return chord:eT()
    end
    if self.equivalence == 'I' then
        return chord:eI()
    end
    if self.equivalence == 'RP' then
        return chord:eRP(self.octaves * ChordSpace.OCTAVE)
    end
    if self.equivalence == 'OP' then
        return chord:eOP()
    end
    if self.equivalence == 'OT' then
        return chord:eOT()
    end
    if self.equivalence == 'OI' then
        return chord:eOI()
    end
    if self.equivalence == 'OPT' then
        return chord:eOPT()
    end
    if self.equivalence == 'OPI' then
        return chord:eOPI()
    end
    if self.equivalence == 'OPTI' then
        return chord:eOPTI()
    end
end

function ChordView:findSize()
    self.minima = self.chords[1]:clone()
    self.maxima = self.chords[1]:clone()
    self.ranges = self.chords[1]:clone()
    for i, chord in ipairs(self.chords) do
        for voice = 1, 3 do
            if self.minima[voice] > chord[voice] then
                self.minima[voice] = chord[voice]
            end
            if self.maxima[voice] < chord[voice] then
                self.maxima[voice] = chord[voice]
            end
        end
    end
    for voice = 1, 3 do
        self.ranges[voice] = self.maxima[voice] - self.minima[voice]
    end
end

function ChordView:startPicking(cursorX, cursorY)
    local viewport = ffi.new('GLint[4]')
	gl.glGetIntegerv(gl.GL_VIEWPORT, viewport);
	gl.glSelectBuffer(ffi.sizeof(self.pickbuffer), self.pickbuffer);
	gl.glRenderMode(gl.GL_SELECT);
    gl.glMatrixMode(gl.GL_PROJECTION);
	gl.glPushMatrix();
	gl.glLoadIdentity();
	glu.gluPickMatrix(cursorX, viewport[3] - cursorY, 1, 1, viewport);
    glu.gluPerspective(45, (viewport[2] - viewport[0]) / (viewport[3] - viewport[1]), 0.1, 1000.0)
	gl.glMatrixMode(gl.GL_MODELVIEW);
	gl.glInitNames();
    gl.glPushName(-1)
end

function ChordView:stopPicking()
	gl.glMatrixMode(gl.GL_PROJECTION);
	gl.glPopMatrix();
	gl.glMatrixMode(gl.GL_MODELVIEW);
	gl.glFlush();
	local hits = gl.glRenderMode(gl.GL_RENDER);
	if hits ~= 0 then
		self:processHits(hits);
    else
        self.pickedChord = nil
    end
end

function ChordView:processHits(hits)
    local i = 0
    local hitsSelected = self.pickbuffer[i]
    i = i + 1
    local hitsMinimumDepth = self.pickbuffer[i]
    i = i + 1
    local hitsMaximumDepth = self.pickbuffer[i]
    i = i + 1
    local hitsName= self.pickbuffer[i]
    i = i + 1
    local pickedDepth = hitsMinimumDepth
    local pickedName = hitsName
    for hit = 1, hits - 1 do
        hitsSelected = self.pickbuffer[i]
        i = i + 1
        hitsMinimumDepth = self.pickbuffer[i]
        i = i + 1
        hitsMaximumDepth = self.pickbuffer[i]
        i = i + 1
        hitsName= self.pickbuffer[i]
        i = i + 1
        if hitsMinimumDepth < pickedDepth then
            pickedDepth = hitsMinimumDepth
            pickedName = hitsName
        end
    end
    self.pickedChord = self.chords[pickedName]
    -- print(string.format('hits: %d  pickedName: %d  pickedDepth: %d', hits, pickedName, pickedDepth))
    -- print(self.pickedChord:__tostring())
    print(self.pickedChord:information())
    print()
end

function ChordView:display()
    local dP = 2.0
    assert( glfw.glfwInit() )
    local window = assert(
      ffi.gc( glfw.glfwCreateWindow( 1024, 768, glfw.GLFW_WINDOWED, "Chord View", nil ),
          glfw.glfwDestroyWindow))
    glfw.glfwDefaultWindowHints()
    glfw.glfwMakeContextCurrent(window)
    glfw.glfwSwapInterval(1)
    self.window = window
    glfw.glfwShowWindow(self.window)
    print('window:', self.window)


    glfw.glfwSetInputMode(window, glfw.GLFW_STICKY_KEYS, gl.GL_TRUE)
    glfw.glfwSetInputMode(window, glfw.GLFW_CURSOR_MODE, glfw.GLFW_CURSOR_NORMAL)
    local redbits = glfw.glfwGetWindowParam(window, glfw.GLFW_RED_BITS)
    local greenbits = glfw.glfwGetWindowParam(window, glfw.GLFW_GREEN_BITS)
    local bluebits = glfw.glfwGetWindowParam(window, glfw.GLFW_BLUE_BITS)
    print('Color bits:', redbits, greenbits, bluebits)
    local intparamptr_t = ffi.typeof('int[1]')
    local newwidth = intparamptr_t()
    local newheight = intparamptr_t()
    local oldwidth = intparamptr_t()
    local oldheight = intparamptr_t()
    local newmousex = intparamptr_t()
    local newmousey = intparamptr_t()
    local oldmousex = intparamptr_t()
    local oldmousey = intparamptr_t()
    local color_t = ffi.typeof('unsigned int[5]')
    local color = color_t()
    glfw.glfwGetWindowSize(window, newwidth, newheight)
    glfw.glfwGetWindowSize(window, oldwidth, oldheight)
    glfw.glfwGetCursorPos(window, newmousex, newmousey)
    glfw.glfwGetCursorPos(window, oldmousex, oldmousey)
    self:resize(newwidth[0], newheight[0])
    self.pickbuffercount = 1000
    self.pickbuffer = ffi.new('int[?]', self.pickbuffercount)
    local tpressed = false
    local ipressed = false
    local ppressed = false
    local rpressed = false
    local lpressed = false
    local dpressed = false
    local kpressed = false
    local qpressed = false
    local _1pressed = false
    local _2pressed = false
    local _3pressed sed = false
    self.modality = Chord:new{0, 4, 7}
    self.drawInverse = false
    local index = 1
    while true do
        -- Check for resizing.
        oldwidth[0] = newwidth[0]
        oldheight[0] = newheight[0]
        glfw.glfwGetWindowSize(window, newwidth, newheight)
        if (newheight[0] ~= oldheight[0]) or (newwidth[0] ~= oldwidth[0]) then
            self:resize(newwidth[0], newheight[0])
        end
        -- Get key input...
        glfw.glfwPollEvents()
        if glfw.glfwGetKey(window, glfw.GLFW_KEY_ESCAPE) == glfw.GLFW_PRESS then
            break
        end
        if glfw.glfwGetKey(window, glfw.GLFW_KEY_C) == glfw.GLFW_PRESS then
            if self.iterateInversions == true then
                self.iterateInversions = false
            else
                self.iterateInversions = true
            end
        end
        -- Zoom in?
        if glfw.glfwGetKey(window, glfw.GLFW_KEY_KP_ADD) == glfw.GLFW_PRESS then
            tz = tz + 3
        end
        -- Zoom out?
        if glfw.glfwGetKey(window, glfw.GLFW_KEY_KP_SUBTRACT) == glfw.GLFW_PRESS then
            tz = tz - 3
        end
        -- Move left?
        if glfw.glfwGetKey(window, glfw.GLFW_KEY_LEFT) == glfw.GLFW_PRESS then
            tx = tx - .1
        end
        -- Move right?
        if glfw.glfwGetKey(window, glfw.GLFW_KEY_RIGHT) == glfw.GLFW_PRESS then
            tx = tx + .1
        end
        -- Move up?
        if glfw.glfwGetKey(window, glfw.GLFW_KEY_UP) == glfw.GLFW_PRESS then
            ty = ty + .1
        end
        -- Move down?
        if glfw.glfwGetKey(window, glfw.GLFW_KEY_DOWN) == glfw.GLFW_PRESS then
            ty = ty - .1
        end
        -- Spin in?
        if glfw.glfwGetKey(window, glfw.GLFW_KEY_KP_9) == glfw.GLFW_PRESS then
            rz = rz - dP
        end
        -- Spin out?
        if glfw.glfwGetKey(window, glfw.GLFW_KEY_KP_3) == glfw.GLFW_PRESS then
            rz = rz + dP
        end
        -- Spin left?
        if glfw.glfwGetKey(window, glfw.GLFW_KEY_KP_7) == glfw.GLFW_PRESS then
            rx = rx - dP
        end
        -- Spin right?
        if glfw.glfwGetKey(window, glfw.GLFW_KEY_KP_1) == glfw.GLFW_PRESS then
            rx = rx + dP
        end
        -- Spin up?
        if glfw.glfwGetKey(window, glfw.GLFW_KEY_KP_8) == glfw.GLFW_PRESS then
            ry = ry - dP
        end
        -- Spin down?
        if glfw.glfwGetKey(window, glfw.GLFW_KEY_KP_2) == glfw.GLFW_PRESS then
            ry = ry + dP
        end
        -- Operate on a chord, if one has been picked, keeping it within the specified orbifold.
        if self.pickedChord ~= nil then
            -- T1?
            if glfw.glfwGetKey(window, glfw.GLFW_KEY_T) == glfw.GLFW_PRESS and not tpressed then
                tpressed = true
                if glfw.glfwGetKey(window, glfw.GLFW_KEY_LEFT_SHIFT) == glfw.GLFW_PRESS then
                    self.pickedChord = self:E(self.pickedChord:T(-1))
                else
                    self.pickedChord = self:E(self.pickedChord:T( 1))
                end
                print(self.pickedChord:information())
                print(ChordSpace.namesForChords[self.pickedChord:information()])
                print()
            end
            if glfw.glfwGetKey(window, glfw.GLFW_KEY_T) == glfw.GLFW_RELEASE then
                tpressed = false
            end
            -- I?
            if glfw.glfwGetKey(window, glfw.GLFW_KEY_I) == glfw.GLFW_PRESS and not ipressed then
                ipressed = true
                local layer = self.pickedChord:layer()
                self.pickedChord = self:E(self.pickedChord:I())
                print(self.pickedChord:information())
                print()
            end
            if glfw.glfwGetKey(window, glfw.GLFW_KEY_I) == glfw.GLFW_RELEASE then
                ipressed = false
            end
            -- P?
            if glfw.glfwGetKey(window, glfw.GLFW_KEY_P) == glfw.GLFW_PRESS and not ppressed then
                ppressed = true
                self.pickedChord = self:E(self.pickedChord:nrP())
                print(self.pickedChord:information())
                print(ChordSpace.namesForChords[self.pickedChord:information()])
                print()
            end
            if glfw.glfwGetKey(window, glfw.GLFW_KEY_P) == glfw.GLFW_RELEASE then
                ppressed = false
            end
            -- L?
            if glfw.glfwGetKey(window, glfw.GLFW_KEY_L) == glfw.GLFW_PRESS and not lpressed then
                lpressed = true
                self.pickedChord = self:E(self.pickedChord:nrL())
                print(self.pickedChord:information())
                print(ChordSpace.namesForChords[self.pickedChord:information()])
                print()
            end
            if glfw.glfwGetKey(window, glfw.GLFW_KEY_L) == glfw.GLFW_RELEASE then
                lpressed = false
            end
            -- R?
            if glfw.glfwGetKey(window, glfw.GLFW_KEY_R) == glfw.GLFW_PRESS and not rpressed then
                rpressed = true
                self.pickedChord = self:E(self.pickedChord:nrR())
                print(self.pickedChord:information())
                print(ChordSpace.namesForChords[self.pickedChord:information()])
                print()
            end
            if glfw.glfwGetKey(window, glfw.GLFW_KEY_R) == glfw.GLFW_RELEASE then
                rpressed = false
            end
            -- D?
            if glfw.glfwGetKey(window, glfw.GLFW_KEY_D) == glfw.GLFW_PRESS and not dpressed then
                dpressed = true
                self.pickedChord = self:E(self.pickedChord:nrD())
                print(self.pickedChord:information())
                print(ChordSpace.namesForChords[self.pickedChord:information()])
                print()
            end
            if glfw.glfwGetKey(window, glfw.GLFW_KEY_D) == glfw.GLFW_RELEASE then
                dpressed = false
            end
            -- K (M)?
            if glfw.glfwGetKey(window, glfw.GLFW_KEY_K) == glfw.GLFW_PRESS and not kpressed then
                kpressed = true
                self.pickedChord = self:E(self.pickedChord:K(self.octaves * 12))
                print(self.pickedChord:information())
                print(ChordSpace.namesForChords[self.pickedChord:information()])
                print()
            end
            if glfw.glfwGetKey(window, glfw.GLFW_KEY_K) == glfw.GLFW_RELEASE then
                kpressed = false
            end
            -- Q1 (M)?
            if glfw.glfwGetKey(window, glfw.GLFW_KEY_Q) == glfw.GLFW_PRESS and not qpressed then
                qpressed = true
                if glfw.glfwGetKey(window, glfw.GLFW_KEY_LEFT_SHIFT) == glfw.GLFW_PRESS then
                    self.pickedChord = self:E(self.pickedChord:Q(1, self.modality))
                else
                    self.pickedChord = self:E(self.pickedChord:Q(-1, self.modality))
                end
                print(self.pickedChord:information())
                print(ChordSpace.namesForChords[self.pickedChord:information()])
                print()
            end
            if glfw.glfwGetKey(window, glfw.GLFW_KEY_Q) == glfw.GLFW_RELEASE then
                qpressed = false
            end
            -- C[1] moves,
            if glfw.glfwGetKey(window, glfw.GLFW_KEY_1) == glfw.GLFW_PRESS and not _1pressed then
                _1pressed = true
                if glfw.glfwGetKey(window, glfw.GLFW_KEY_LEFT_SHIFT) == glfw.GLFW_PRESS then
                    self.pickedChord = self:E(self.pickedChord:move(1, -1))
                else
                    self.pickedChord = self:E(self.pickedChord:move(1,  1))
                end
                print(self.pickedChord:information())
                print(ChordSpace.namesForChords[self.pickedChord:information()])
                print()
            end
            if glfw.glfwGetKey(window, glfw.GLFW_KEY_1) == glfw.GLFW_RELEASE then
                _1pressed = false
            end
            -- C[2] moves,
            if glfw.glfwGetKey(window, glfw.GLFW_KEY_2) == glfw.GLFW_PRESS and not _2pressed then
                _2pressed = true
                if glfw.glfwGetKey(window, glfw.GLFW_KEY_LEFT_SHIFT) == glfw.GLFW_PRESS then
                    self.pickedChord = self:E(self.pickedChord:move(2, -1))
                else
                    self.pickedChord = self:E(self.pickedChord:move(2,  1))
                end
                print(self.pickedChord:information())
                print(ChordSpace.namesForChords[self.pickedChord:information()])
                print()
            end
            if glfw.glfwGetKey(window, glfw.GLFW_KEY_2) == glfw.GLFW_RELEASE then
                _2pressed = false
            end
            -- C[3] moves,
            if glfw.glfwGetKey(window, glfw.GLFW_KEY_3) == glfw.GLFW_PRESS and not _3pressed then
                _3pressed = true
                if glfw.glfwGetKey(window, glfw.GLFW_KEY_LEFT_SHIFT) == glfw.GLFW_PRESS then
                    self.pickedChord = self:E(self.pickedChord:move(3, -1))
                else
                    self.pickedChord = self:E(self.pickedChord:move(3,  1))
                end
                print(self.pickedChord:information())
                print(ChordSpace.namesForChords[self.pickedChord:information()])
                print()
            end
            if glfw.glfwGetKey(window, glfw.GLFW_KEY_3) == glfw.GLFW_RELEASE then
                _3pressed = false
            end
            -- Voicelead from prior to current?
        end
        -- Get mouse input...
        -- Change modality?
        if glfw.glfwGetKey(window, glfw.GLFW_KEY_M) == glfw.GLFW_PRESS and not mpressed then
            mpressed = true
            self.modality = self:E(self.modality:nrP())
            print(string.format('Modality: %s', self.modality:__tostring()))
        end
        if glfw.glfwGetKey(window, glfw.GLFW_KEY_M) == glfw.GLFW_RELEASE then
            mpressed = false
        end
        oldmousex[0] = newmousex[0]
        oldmousey[0] = newmousey[0]
        glfw.glfwGetCursorPos(window, newmousex, newmousey)
        local button = glfw.glfwGetMouseButton(window, glfw.GLFW_MOUSE_BUTTON_LEFT)
        if button == glfw.GLFW_PRESS then
            self:startPicking(newmousex[0], newmousey[0])
            self:draw(true)
            self:stopPicking()
        else
            if self.iterateInversions == true then
                if self.drawInverse == false then
                    if index > #self.chords then
                        index = 1
                    end
                    self.pickedChord = self.chords[index]
                    print(string.format('%4d chord:    %s  iseOPI: %5s  iseOPT: %s', index, tostring(self.pickedChord), tostring(self.pickedChord:iseOPI()), tostring(self.pickedChord:iseOPT())))
                    print(string.format('     flatP:    %s', tostring(self.pickedChord:flatP())))
                    print(string.format('     flatRP:   %s', tostring(self.pickedChord:flatRP())))
                    print(string.format('     midpoint: %s', tostring(self.pickedChord:inversionMidpoint())))
                    self:draw(false)
                    self.drawInverse = true
                else
                    local inverse = self.chords[index]:I():eOP()
                    print(string.format('     inverse:  %s  iseOPI: %5s  iseOPT: %s', tostring(inverse), tostring(inverse:iseOPI()), tostring(self.pickedChord:iseOPT())))
                    if inverse == self.pickedChord then
                        if not (inverse:iseOPI() and self.pickedChord:iseOPI()) then
                            print("Error: a fixed point that is not in OPI.")
                            os.exit()
                        end
                    else
                        if inverse:iseOPI() == self.pickedChord:iseOPI() then
                            print("Error: chord and inverse are either both in or both out of OPI.")
                            os.exit()
                        end
                    end

                    -- If it is not a fixed point one must be iseOPI and the other not.
                    self.pickedChord = inverse
                    print()
                    self:draw(false)
                    self.drawInverse = false
                    index = index + 1
                end
            else
                self:draw(false)
            end
        end
    end
end

ChordSpaceView.help()

chordView = ChordView:new()
chordView.iterateInversions = false
chordView.octaves = 1
chordView.equivalence = 'OPTTI'
chordView.constructChordsByOperation = false
chordView:createChords()
chordView:findSize()
chordView:display()

return ChordSpaceView



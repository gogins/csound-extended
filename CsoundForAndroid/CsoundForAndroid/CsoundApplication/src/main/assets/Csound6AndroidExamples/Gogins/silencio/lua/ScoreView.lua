local ScoreView = {}
function ScoreView.help()
print [[
What we want here is the following:
(1) Display a 3-dimensional piano roll view of a score:
    Time -- X axis
    Pitch -- Y axis
    Instrument -- Z axis AND hue
    Loudness -- value (brightness)
(2) Notes are flat 'boards' with thickness (25 cents), width (1 instrument number), and length (duration in seconds).
(3) Notes are displayed within a grid of semi-transparent lines denoting time, pitch, and instrument spaced by MIDI key number,
    second, and instrument number. The origin should be denoted by a ball, and 10 second and octave ticks should be denoted by smaller balls.
(4) It should be possible to pick notes using the mouse; doing so will toggle a display of the actual values of the note and/or play the note.
(5) The user should be able to navigate in the score by translating and rotating on all 3 dimensions.
]]
end

local ffi =         require("ffi")
local gl   =        require("ffi/OpenGL" )
local glu  =        require("ffi/glu")
local glfw =        require("ffi/glfw")
local bit  =        require("bit")

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

ScoreViewer = {}

function ScoreViewer:new(o)
    local o = o or {score = nil, title = 'Score View', fullscreen = true, minima = {}, maxima = {}, scales = {}, pickedNote = nil}
    setmetatable(o, self)
    self.__index = self
    return o
end

function ScoreViewer:material()
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

function ScoreViewer:draw(picking)
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
    for i, note in ipairs(self.score) do
        self:drawNote(note)
    end
    if not picking then
        glfw.glfwSwapBuffers(self.window)
    end
end

function ScoreViewer:drawGrid()
    gl.glBegin(gl.GL_LINES)
    for xi, x in ipairs(self.gridXs) do
        for yi, y in ipairs(self.gridYs) do
            for zi, z in ipairs(self.gridZs) do
                if y - self.beginY == 36 or x == self.beginX then
                    gl.glColor4f(1, 0, 0, 0.5)
                else
                    gl.glColor4f(0.25, 0.25, 0.25, 0.75)
                end
                gl.glVertex3f(self.beginX, y, z)
                gl.glVertex3f(self.endX, y, z)
                gl.glVertex3f(x, self.beginY, z)
                gl.glVertex3f(x, self.endY, z)
                gl.glVertex3f(x, y, self.beginZ)
                gl.glVertex3f(x, y, self.endZ)
            end
        end
    end
    gl.glEnd()
end 

function ScoreViewer:resize(width, height)
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
    self.scales = self.score:findScales()
    self.beginX = self.scales[1][TIME]
    self.beginY = self.scales[1][KEY]
    self.beginZ = self.scales[1][CHANNEL]
    self.sizeX = self.scales[2][TIME]
    self.sizeY = self.scales[2][KEY]
    self.sizeZ = self.scales[2][CHANNEL]
    self.endX = self.beginX + self.sizeX
    self.endY = self.beginY + self.sizeY
    self.endZ = self.beginZ + self.sizeZ
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
    glu.gluPerspective(45, self.aspect, 1, 500)
    print(string.format('Center: x: %9.4f, y: %9.4f, z: %9.4f', self.centerX, self.centerY, self.centerZ))
    print(string.format('Front: %9.4f, back: %9.4f', self.front, self.back))
    gl.glMatrixMode(gl.GL_MODELVIEW)
    gl.glLoadIdentity()
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
end

function ScoreViewer:startPicking(cursorX, cursorY) 
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

function ScoreViewer:stopPicking()
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

function ScoreViewer:processHits(hits)
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
    print(string.format('hits: %d  pickedName: %d  pickedDepth: %d', hits, pickedName, pickedDepth))
    print(self.pickedChord:__tostring())
    print(self.pickedChord:label())
    print()
end

function ScoreViewer:display()
    assert( glfw.glfwInit() )
    local window = assert(
      ffi.gc( glfw.glfwCreateWindow( 1024, 768, glfw.GLFW_WINDOWED, "Chord View", nil ),
          glfw.glfwDestroyWindow))
    glfw.glfwDefaultWindowHints()
    glfw.glfwMakeContextCurrent(window)
    glfw.glfwSwapInterval(1)
    self.window = window
    glfw.glfwShowWindow(self.window)
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
    local _3pressed = false    
    local mpressed = false
    while true do
        glfw.glfwPollEvents()
        -- Check for resizing.
        oldwidth[0] = newwidth[0]
        oldheight[0] = newheight[0]
        glfw.glfwGetWindowSize(window, newwidth, newheight)
        if (newheight[0] ~= oldheight[0]) or (newwidth[0] ~= oldwidth[0]) then
            self:resize(newwidth[0], newheight[0])
        end
        -- Get key input...
       if glfw.glfwGetKey(window, glfw.GLFW_KEY_ESCAPE) == glfw.GLFW_PRESS then
            break
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
            rz = rz - .7
        end
        -- Spin out?
        if glfw.glfwGetKey(window, glfw.GLFW_KEY_KP_3) == glfw.GLFW_PRESS then
            rz = rz + .7
        end
        -- Spin left?
        if glfw.glfwGetKey(window, glfw.GLFW_KEY_KP_7) == glfw.GLFW_PRESS then
            rx = rx - .7
        end
        -- Spin right?
        if glfw.glfwGetKey(window, glfw.GLFW_KEY_KP_1) == glfw.GLFW_PRESS then
            rx = rx + .7
        end
        -- Spin up?
        if glfw.glfwGetKey(window, glfw.GLFW_KEY_KP_8) == glfw.GLFW_PRESS then
            ry = ry - .7
        end
        -- Spin down?
        if glfw.glfwGetKey(window, glfw.GLFW_KEY_KP_2) == glfw.GLFW_PRESS then
            ry = ry + .7
        end
        -- Operate on a note, if one has been picked.
        if self.pickedNote ~= nil then
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
            self:draw(false)
        end
    end
end

function ScoreViewer:drawNote(note)
    gl.glPushMatrix()
    gl.glTranslatef(note[TIME], note[KEY], note[CHANNEL])
    gl.glBegin(gl.GL_QUADS)
    local hue = (note[CHANNEL] - self.scales[1][CHANNEL]) / self.scales[2][CHANNEL] 
    hue = 60 + hue * 300
    local saturation = 1
    local value = (note[VELOCITY] - self.scales[1][VELOCITY]) / self.scales[2][VELOCITY] 
    value = 0.5 + (value * 0.5)
    local alpha = 0.5
    local r, g, b = hsv_to_rgb(hue, value, value, alpha)
    --print('hsv:', hue, saturation, value    )
    gl.glColor3f(r or .5, g or .5, b or .5)
    local d = note[DURATION]
    local w = 1
    local t = 0.25
    -- Front Face
    gl.glNormal3f( 0,  0,  1)
    gl.glVertex4f( 0,  0,  w, 1)
    gl.glVertex4f( d,  0,  w, 1)
    gl.glVertex4f( d,  t,  w, 1)
    gl.glVertex4f( 0,  t,  w, 1)
    -- Back Face
    gl.glNormal3f( 0,  0, -1)
    gl.glVertex4f( 0,  0,  0, 1)
    gl.glVertex4f( d,  0,  0, 1)
    gl.glVertex4f( d,  t,  0, 1)
    gl.glVertex4f( 0,  t,  0, 1)
    -- Top Face
    gl.glNormal3f( 0,  1,  0)
    gl.glVertex4f( 0,  t,  0, 1)
    gl.glVertex4f( d,  t,  0, 1)
    gl.glVertex4f( d,  t,  w, 1)
    gl.glVertex4f( 0,  t,  w, 1)
    -- Bottom Face
    gl.glNormal3f( 0, -1,  0)
    gl.glVertex4f( 0,  0,  0, 1)
    gl.glVertex4f( d,  0,  0, 1)
    gl.glVertex4f( d,  0,  w, 1)
    gl.glVertex4f( 0,  0,  w, 1)
    -- Right Face
    gl.glNormal3f( 1,  0,  0)
    gl.glVertex4f( d,  0,  0, 1)
    gl.glVertex4f( d,  t,  0, 1)
    gl.glVertex4f( d,  t,  w, 1)
    gl.glVertex4f( d,  0,  w, 1)
    -- Left Face
    gl.glNormal3f(-1,  0,  0)
    gl.glVertex4f( 0,  0,  0, 1)
    gl.glVertex4f( 0,  t,  0, 1)
    gl.glVertex4f( 0,  t,  w, 1)
    gl.glVertex4f( 0,  0,  w, 1)

    gl.glEnd()
    gl.glPopMatrix()
end

function ScoreView.display(score_)
    print('BEGAN ScoreView.display()...')
    print(score_)
    local scoreViewer = ScoreViewer:new()
    scoreViewer.score = score_
    scoreViewer:display()
    print('ENDED ScoreView.display().')
end

return ScoreView



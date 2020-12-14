/**
 * A helper class that displays a CsoundAC Score in a 3-dimensional piano roll.
 */
function PianoRoll() {
    this.score = null;
    this.canvas = null;
    this.context = null;
    this.scene = null;
    this.camera = null;
    this.renderer = null;
    this.controls = null;
    this.score_cursor = null;
}

PianoRoll.prototype.progress = function(score_time) {
    if (context !== null) {
        context.fillStyle = "LawnGreen";
        context.fillRect(0, 60, score_time, 0.01);
    }
};

/**
 * Displays the score cursor at the current time.
 */
PianoRoll.prototype.progress3D = function(score_time) {
    if (this.scene !== null) {
        this.score_cursor.position.x = score_time;
        this.score_cursor.position.y = 60;
        this.score_cursor.position.z = 0.5;
        this.controls.update();
        this.camera.updateProjectionMatrix();
        this.renderer.render(this.scene, this.camera);
    }
};

/**
 * Sets up a scene, camera, and renderer with controls to view
 * either a fixed or a real-time score.
 */
PianoRoll.prototype.prepareScene3D = function() {
    var canvas = this.canvas;
    canvas.width = canvas.clientWidth;
    canvas.height = canvas.clientHeight;
    this.score.findScale();
    this.scene = new THREE.Scene();
    var scene = this.scene;
    this.renderer = new THREE.WebGLRenderer({
        canvas: this.canvas,
        antialias: true
    });
    var renderer = this.renderer;
    renderer.setClearColor(0);
    renderer.sortObjects = false;
    renderer.setViewport(0, 0, canvas.clientWidth, canvas.clientHeight);
    renderer.setPixelRatio(canvas.devicePixelRatio);
    // Wire up the view controls to the camera.
    this.camera = new THREE.PerspectiveCamera(45, canvas.clientWidth / canvas.clientHeight, 1, 10000);
    var camera = this.camera;
    this.controls = new THREE.TrackballControls(camera, renderer.domElement);
    var controls = this.controls;
    controls.rotateSpeed = 1.0;
    controls.zoomSpeed = 1;
    controls.panSpeed = 1;
    controls.noZoom = false;
    controls.noPan = false;
    controls.staticMoving = true;
    controls.dynamicDampingFactor = 0.3;
    //controls.keys = [ 65, 83, 68 ];
    this.canvas.addEventListener("mousemove", function(e) {
        piano_roll.render3D();
    });
    // Ensure that all sides are lighted.
    var light = new THREE.DirectionalLight(0xffffff, 1);
    light.position.set(1, 1, 1).normalize();
    this.scene.add(light);
    var light2 = new THREE.AmbientLight(0x404040, 0.5);
    this.scene.add(light2);
    var onResize = function() {
        canvas.width = canvas.clientWidth;
        canvas.height = canvas.clientHeight;
        this.renderer.setViewport(0, 0, canvas.clientWidth, canvas.clientHeight);
        this.camera.aspect = canvas.clientWidth / canvas.clientHeight;
        this.controls.handleResize();
        this.camera.updateProjectionMatrix();
        this.renderer.render(this.scene, this.camera);
    };
    window.addEventListener('resize', onResize, false);
};

/**
 * Adds the note to the 3D scene. Can be used with a fixed or a real-time score.
 */
PianoRoll.prototype.plotNote3D = function(note) {
    var begin = note.getTime();
    var end = note.getOffTime();
    var duration = end - begin;
    var key = note.getKey();
    var channel = note.getChannel() - this.channel_minimum;
    var geometry = new THREE.BoxBufferGeometry(duration, 1, 1);
    var channel_range = this.channel_range;
    if (channel_range === 0) {
        channel_range = 1;
    }
    var velocity_range = this.velocity_range;
    if (velocity_range === 0) {
        velocity_range = 1;
    }
    var hue = channel / channel_range;
    var velocity = note.getVelocity();
    var velocity = velocity - this.velocity_minimum;
    var value = velocity / velocity_range;
    value = 0.5 + value / 2;
    var material = new THREE.MeshLambertMaterial();
    material.color.setHSL(hue, 1, value);
    material.opacity = 0.5;
    material.reflectivity = 0.5;
    material.transparent = true;
    material.emissive = material.color;
    material.emissiveIntensity = 2 / 3;
    var note_mesh = new THREE.Mesh(geometry, material);
    note_mesh.position.x = begin + duration / 2; // + note.scale.x;
    note_mesh.position.y = key;
    note_mesh.position.z = channel;
    this.scene.add(note_mesh);
};

/**
 * Plots a grid for a fixed score.
 */
PianoRoll.prototype.plotGrid3D = function() {
    // Generate the grid. Its origin for time is 0 and for pitch its origin is the
    // first C lower than or equal to the lowest pitch in the score.
    var time_minimum = this.time_minimum;
    var time_maximum = this.time_minimum + this.time_range;
    var key_minimum = this.key_minimum;
    var key_maximum = this.key_minimum + this.key_range;
    var channel_minimum = this.channel_minimum;
    var channel_maximum = this.channel_maximum;
    var line_material = new THREE.LineBasicMaterial();
    time_minimum = 0;
    instrument_minimum = 0;
    if (key_minimum % 12 !== 0) {
        key_minimum -= (key_minimum % 12);
    }
    var grid_geometry = new THREE.BoxBufferGeometry(10, 12, 1);
    for (var t = time_minimum; t <= time_maximum + 10; t = t + 10) {
        for (var k = key_minimum; k <= key_maximum; k = k + 12) {
            var box = new THREE.LineSegments(new THREE.EdgesGeometry(grid_geometry), line_material);
            ///box = new THREE.EdgesGeometry(box);
            box.material.color.setRGB(0, .5, 0);
            box.material.opacity = 0.25;
            box.material.transparent = true;
            box.position.x = t + 5;
            box.position.y = k + 6;
            box.position.z = 0;
            box.scale.z = 0;
            this.scene.add(box);
        }
    }
    // Put a ball at the origin, to indicate the orientation of the score.
    var origin_geometry = new THREE.SphereGeometry(1, 10, 10);
    var origin_material = new THREE.MeshLambertMaterial();
    origin_material.color.setRGB(0, 255, 0);
    var origin = new THREE.Mesh(origin_geometry, origin_material);
    origin.position.x = time_minimum;
    origin.position.y = key_minimum;
    origin.position.z = 0;
    this.scene.add(origin);
    // Put a ball at the start of middle C, to indicate the current Csound
    // score time.
    var cursor_geometry = new THREE.SphereGeometry(1, 10, 10);
    var cursor_material = new THREE.MeshLambertMaterial();
    cursor_material.color.setRGB(255, 0, 0);
    this.score_cursor = new THREE.Mesh(cursor_geometry, cursor_material);
    this.score_cursor.position.x = time_minimum;
    this.score_cursor.position.y = 60;
    this.score_cursor.position.z = 0;
    this.scene.add(this.score_cursor);
};

/**
 * Looks at a full fixed score.
 */
PianoRoll.prototype.lookAtFullScore3D = function() {
    var bounding_box = new THREE.Box3().setFromObject(this.scene);
    this.camera.lookAt(bounding_box.getCenter());
    this.camera.fov = 2 * Math.atan((bounding_box.getSize().x / (this.canvas.width / this.canvas.height)) / (2 * bounding_box.getSize().y)) * (180 / Math.PI);
    this.camera.position.copy(bounding_box.getCenter());
    this.camera.position.z = 1.125 * Math.min(bounding_box.getSize().x, bounding_box.getSize().y);
    this.controls.target.copy(bounding_box.getCenter());
    this.controls.update();
    this.camera.updateProjectionMatrix();
    this.renderer.render(this.scene, this.camera);
};

/**
 * Looks at the front (current notes) of a real-time score.
 */
PianoRoll.prototype.lookAtFront3D = function() {
    var bounding_box = new THREE.Box3().setFromObject(this.scene);
    this.camera.lookAt(bounding_box.getCenter());
    this.camera.fov = 2 * Math.atan((bounding_box.getSize().y / (this.canvas.width / this.canvas.height)) / (2 * bounding_box.getSize().z)) * (180 / Math.PI);
    this.camera.position.copy(bounding_box.getCenter());
    this.camera.position.x = 1.125 * Math.max(bounding_box.getSize().x, bounding_box.getSize().y);
    this.controls.target.copy(bounding_box.getCenter());
    this.controls.update();
    this.camera.updateProjectionMatrix();
    this.renderer.render(this.scene, this.camera);
};

/**
 * Redraws the scene using the camera updated from the controls.
 */
PianoRoll.prototype.render3D = function() {
    this.controls.update();
    this.camera.updateProjectionMatrix();
    this.renderer.render(this.scene, this.camera);
};

/**
 * Draws the notes in a fixed score as a 3-dimensional piano roll. The score is
 * fitted into the viewport to start with, but the user can use the mouse or
 * trackball to move around the score and to zoom in and out. The dimensions
 * are: time = x, MIDI key = y, MIDI channel = z and hue, and loudness =
 * value; a grid shows tens of seconds and octaves.
 */
PianoRoll.prototype.draw3D = function(score_, canvas_) {
    this.score = score_;
    this.canvas = canvas_;
    this.score.findScale();
    this.time_minimum = this.score.getScaleActualMinima().getTime();
    this.time_range = this.score.getDuration();
    this.channel_minimum = this.score.getScaleActualMinima().getChannel();
    this.channel_range = this.score.getScaleActualRanges().getChannel();
    this.channel_maximum = this.channel_minimum + this.channel_range;
    this.key_minimum = this.score.getScaleActualMinima().getKey();
    this.key_range = this.score.getScaleActualRanges().getKey();
    this.key_maximum = this.key_minimum + this.key_range;
    this.velocity_minimum = this.score.getScaleActualMinima().getVelocity();
    this.velocity_range = this.score.getScaleActualRanges().getVelocity();
    this.prepareScene3D(canvas_, score_);
    // Plot the notes.
    let n = this.score.size();
    for (var i = 0; i < n; i++) {
        let event = this.score.get(i);
        this.plotNote3D(event);
    }
    this.plotGrid3D();
    this.lookAtFullScore3D();
    return canvas;
};

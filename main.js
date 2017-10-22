var main = () => {
    // set cavnas
    var canvas = document.getElementById("newCanvas");
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;

    // get context
    var ctx = canvas.getContext('2d');
    ctx.lineWidth = 1;
    
    // call world 
    var ri = 0.0531; // h / 2 : (m / ρ)^1/Dimension
    var timeStep = 0.004;
    var rho0 = 0.2524;
    var world = new World(timeStep, ri, rho0);
    
    // set ratio
    var pixPerMeter = 800;

    // set user's input
    document.addEventListener("keydown", keyDownFunc);
    document.addEventListener("keyup", keyUpFunc);
    var leftFlg, rightFlg, upFlg, downFlg;
    leftFlg = rightFlg = upFlg = downFlg = false;

    // flags related to drawing
    var drawRadius = 1;
    var drawMesh = 0;
    var drs = _ => { drawRadius ^= true; };
    var drm = _ => { drawMesh ^= true; };
    
    function confine(){
        var ratio = 1.1;
        Ary.map(particle => {
            if(abs(particle.x) > canvas.width / 2 / pixPerMeter * ratio){
                particle.x *= -1/ratio;
            }
            if(abs(particle.y) > canvas.height / 2 / pixPerMeter * ratio){
                particle.y *= -1/ratio;
            }
        }, world.particles);
    }

    function resizeStr(num){
        num = num.toString(16);
        if(num.length < 2) num = '0' + num;
        return num;
    }

    function colorScale(value){
        var r = floor(min(255, max(0, 1024*(value-0.5))));
        var g = floor(min(255, max(0, 512-abs(1024*(value-0.5)))));
        var b = floor(min(255, max(0, 1024*(-value+0.25))));
        var color = '#' + resizeStr(r) + resizeStr(g) + resizeStr(b);
        return color;
    }

    function keyDownFunc(e){
        if (e.keyCode == 37) leftFlg  = true;
        if (e.keyCode == 38) upFlg    = true;
        if (e.keyCode == 40) downFlg  = true;
        if (e.keyCode == 39) rightFlg = true;
    }

    function keyUpFunc(e){
        if (e.keyCode == 37) leftFlg  = false;
        if (e.keyCode == 38) upFlg    = false;
        if (e.keyCode == 40) downFlg  = false;
        if (e.keyCode == 39) rightFlg = false;
    }

    var fillRect = (x, y, w, h, theta) => {
        ctx.fillStyle = "black";
        ctx.translate(x, y);
        ctx.rotate(theta);
        ctx.fillRect(-w/2, -h/2, w, h);
        ctx.rotate(-theta);
        ctx.translate(-x, -y);
    }

    var userInput = _ => {
        world.elecX = 0;
        world.elecY = 0;
        var dE = 0.01;
        
        if(rightFlg) world.k += 0.001;
        if(leftFlg) world.k -= 0.001;
        if(upFlg) world.rho0 += 0.0001;
        if(downFlg) world.rho0 -= 0.0001;
        world.rho0 = floor(world.rho0 * 100000) / 100000;
        world.k    = floor(world.k    * 10000) / 10000;
    }

    function draw(){
        ctx.fillStyle = "white";
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        //var colors = ;
        Ary.map(wall => {
            var x = canvas.width / 2 + wall.x * pixPerMeter;
            var y = canvas.height / 2 + wall.y * pixPerMeter;
            var theta = wall.a;
            var w = wall.w * pixPerMeter;
            var h = wall.h * pixPerMeter;
            fillRect(x, y, w, h, theta);
        }, world.walls);
        Ary.map(particle => {
            var x = canvas.width / 2 + particle.x * pixPerMeter;
            var y = canvas.height / 2 + particle.y * pixPerMeter;
            var r = particle.r * pixPerMeter;
            var ri = world.ri * pixPerMeter;
            var rho = min(1, max(0, (particle.rho - 1.38) * 1.4));
            ctx.beginPath();
            ctx.fillStyle = colorScale(rho);
            ctx.arc(x, y, r, 0, TWO_PI, false);
            ctx.fill();
            ctx.globalAlpha = 0.4;
            if(drawRadius){
                ctx.beginPath();
                ctx.arc(x, y, ri, 0, TWO_PI, false);
                ctx.stroke();
            }
            ctx.globalAlpha = 1.0;
        }, world.particles);
        
        ctx.fillStyle = "black";
        ctx.font = "20px 'Courier New'";
        ctx.fillText("rho : " + world.rho0, 30, 30);
        ctx.fillText("k   : " + world.k   , 30, 60);

        if(drawMesh){
            for(var i = 0; i < 30; ++i){
                ctx.beginPath();
                ctx.moveTo(canvas.width / 2 + (-15 + i) * ri * 2 * pixPerMeter, 0);
                ctx.lineTo(canvas.width / 2 + (-15 + i) * ri * 2 * pixPerMeter, canvas.height);
                ctx.stroke();
            }
            for(var i = 0; i < 30; ++i){
                ctx.beginPath();
                ctx.moveTo(0,  canvas.height / 2 +(-15 + i) * ri * 2* pixPerMeter);
                ctx.lineTo(canvas.width,  canvas.height / 2 + (-15 + i) * ri * 2 * pixPerMeter);
                ctx.stroke();
            }
        }
    }
    
    function init(){
        var w = canvas.width / pixPerMeter / 3;
        var h = canvas.height / pixPerMeter / 3;
        for(var i = 0; i < 5; i+=0.5){
            for(var j = 0; j < 10; j+=0.5){
                var x = (-6.0 + i) * 0.040;
                var y = (-4.0 + j) * 0.040;
                world.addParticle(new Particle(x, y, false));
            }
        }
        
        var r = min(canvas.width, canvas.height) / pixPerMeter / 3;
        world.addWall(new Wall(0, r, 0.12, 2.0, PI / 2));
        world.addWall(new Wall(r, 0, 0.12, 0.9, 0));
        world.addWall(new Wall(-r, 0, 0.12, 0.9, 0));
    }
    
    function moveWall(){
        var omega = 0.002;
        var r = 0.5;
        /*Ary.map(wall => {
            var theta = wall.a;
            theta += omega;
            wall.a = theta;
            wall.x = r * cos(theta);
            wall.y = r * sin(theta);
        }, world.walls);*/
    }

    function step(){
        //moveWall();
        userInput();
        world.step();
        confine();
    }
    
    init();
    setInterval(() => {
        step();
        draw();
    }, 8);
    
    return { world : world, 
             drs : drs,
             drm : drm,
    };
};

var root = main();


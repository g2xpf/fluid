var main = () => {
    // set cavnas
    var canvas = document.getElementById("newCanvas");
    var borderWidth = 2;
    canvas.width = window.innerWidth - 2 * borderWidth;
    canvas.height = window.innerHeight - 2 * borderWidth;
    canvas.style.position = "absolute";
    canvas.style.top = "0px";
    canvas.style.left = "0px";
    canvas.style.border = borderWidth + "px solid #000";

    // get context
    var ctx = canvas.getContext('2d');
    ctx.lineWidth = 0.6;
    
    // call world 
    var ri = 0.024; // h / 2 : (m / ρ)^1/Dimension
    var timeStep = 0.004;
    var rho0 = 80;//82.66;
    var world = new World(timeStep, ri, rho0);
    var state = 0;
    // set ratio
    var pixPerMeter = max(canvas.width, canvas.height);

    // set user's input
    document.addEventListener("keydown", keyDownFunc);
    document.addEventListener("keyup", keyUpFunc);
    var leftFlg, rightFlg, upFlg, downFlg, aFlg, dFlg;
    leftFlg = rightFlg = upFlg = downFlg = aFlg = dFlg = false;

    // flags related to drawing
    var drawRadius = 0;
    var drawMesh = 0;
    var drs = _ => { drawRadius ^= true; };
    var drm = _ => { drawMesh ^= true; };
    
    

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
        if (e.keyCode == 65) aFlg = true;
        if (e.keyCode == 68) dFlg = true;
    }

    function keyUpFunc(e){
        if (e.keyCode == 37) leftFlg  = false;
        if (e.keyCode == 38) upFlg    = false;
        if (e.keyCode == 40) downFlg  = false;
        if (e.keyCode == 39) rightFlg = false;
        if (e.keyCode == 65) aFlg = false;
        if (e.keyCode == 68) dFlg = false;
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
        var dE = 20;
        
        /*if(rightFlg) world.k += 0.001;
        if(leftFlg) world.k -= 0.001;
        if(upFlg) world.rho0 += 0.0001;
        if(downFlg) world.rho0 -= 0.0001;
        world.rho0 = floor(world.rho0 * 100000) / 100000;
        world.k    = floor(world.k    * 10000) / 10000;*/
        if(rightFlg) world.elecX += dE;  
        if(leftFlg)  world.elecX -= dE; 
        if(upFlg)    world.elecY -= dE; 
        if(downFlg)  world.elecY += dE; 
        if(dFlg){
            state = 1;
            world = new World(timeStep, ri, rho0); 
            init();
        }else if(aFlg){
            state = 0;
            world = new World(timeStep, ri, rho0); 
            init();
        }
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
            var rho = min(1, max(0, (particle.rho - rho0) * 0.014));
            ctx.beginPath();
            ctx.fillStyle = colorScale(rho);
            ctx.arc(x, y, r, 0, TWO_PI, false);
            ctx.fill();
            ctx.globalAlpha = 0.7;
            if(drawRadius){
                ctx.beginPath();
                ctx.arc(x, y, ri, 0, TWO_PI, false);
                ctx.stroke();
            }
            ctx.globalAlpha = 1.0;
        }, world.particles);
        
        ctx.fillStyle = "black";
        ctx.font = "20px 'Courier New'";
        //ctx.fillText("rho : " + world.rho0, 30, 30);
        //ctx.fillText("k   : " + world.k   , 30, 60);

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
        switch(state){
            case 0:
                var w = canvas.width / pixPerMeter / 3;
                var h = canvas.height / pixPerMeter / 3;
                var d = world.d * 0.87 / world.scale;
                var x0 = -15.0, y0 = 0.0, z0 = 0.0;
                var x1 = 15.0, y1 = 20.0, z1 = 0.0; 
                var ratio = 1.53;
                console.log(d);
                for(var z = z0; z <= z1; z += d)
                    for(var y = y0; y <= y1; y += d)
                        for(var x = x0; x <= x1; x += d){
                            x += 0.01 * Math.random();
                            y += 0.01 * Math.random();
                            z += 0.01 * Math.random();
                            world.addParticle(new Particle(x * world.scale * ratio, y * world.scale * ratio, z * world.scale * ratio, false));
                        }

                var r = min(canvas.width, canvas.height) / pixPerMeter / 3;
                world.makeMesh();
                /*world.addWall(new Wall(0, -0.3, 0, 20, 0.3, 0));
                  world.addWall(new Wall(0.65, 0, 0, 0.9, 20, 0));
                  world.addWall(new Wall(-0.65, 0, 0, 0.9, 20, 0));
                  world.addWall(new Wall(0, 0.6, 0, 20, 0.9, 0));*/

                //addFrame(200, 90, 0.9);
                addCircle(20, 200, 2.0);
                //addFrame(52, 32, 1.702);
                //addFrame(54, 34, 1.704);
                break;
            case 1:
                var w = canvas.width / pixPerMeter / 3;
                var h = canvas.height / pixPerMeter / 3;
                var d = world.d * 0.87 / world.scale;
                var x0 = -43.0, y0 = 0.0, z0 = 0.0;
                var x1 = 43.0, y1 = 20.0, z1 = 0.0; 
                var ratio = 1.53;
                console.log(d);
                for(var z = z0; z <= z1; z += d)
                    for(var y = y0; y <= y1; y += d)
                        for(var x = x0; x <= x1; x += d){
                            x += 0.01 * Math.random();
                            y += 0.01 * Math.random();
                            z += 0.01 * Math.random();
                            world.addParticle(new Particle(x * world.scale * ratio, y * world.scale * ratio, z * world.scale * ratio, false));
                        }

                var r = min(canvas.width, canvas.height) / pixPerMeter / 3;
                world.makeMesh();
                /*world.addWall(new Wall(0, -0.3, 0, 20, 0.3, 0));
                world.addWall(new Wall(0.65, 0, 0, 0.9, 20, 0));
                world.addWall(new Wall(-0.65, 0, 0, 0.9, 20, 0));
                world.addWall(new Wall(0, 0.6, 0, 20, 0.9, 0));*/

                //addFrame(200, 90, 0.9);
                //addCircle(20, 200, 2.0);
                //addFrame(52, 32, 1.702);
                //addFrame(54, 34, 1.704);
                break;
            default:
                break;
        }
    }
    
    function addFrame(width, height, ratio){
        var xBox = [];
        var yBox = [];
        for(var i = 0; i < width; ++i){
            xBox.push(i);
            yBox.push(0);
            xBox.push(i);
            yBox.push(height-1);
        }
        for(var i = 1; i < height - 1; ++i){
            xBox.push(0);
            yBox.push(i);
            xBox.push(width-1);
            yBox.push(i);
        }
        var getX = x => (x - width / 2) * ratio;
        var getY = y => (y - height / 2) * ratio;
        for(var i = 0, len = xBox.length; i < len; ++i){
            var x = getX(xBox[i]);
            var y = getY(yBox[i]);
            var z = 0;
            world.addParticle(new Particle(x * world.scale * ratio, y * world.scale * ratio, z * world.scale * ratio, true));
        }
    }

    function addCircle(r, splitNum, ratio){
        for(var i = 0; i < splitNum; ++i){
            var theta = i * TWO_PI / splitNum;
            var x = r * cos(theta);
            var y = r * sin(theta);
            var z = 0;
            world.addParticle(new Particle(x * world.scale * ratio, y * world.scale * ratio, z * world.scale * ratio, true));
        }
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
        world.confine(canvas.width, canvas.height, pixPerMeter);
    }
    
    var time = (function* (){
        var i = 0;
        var start = new Date(), end;
        var t, timeCount = 30, log;
        while(1){
            if(i++ % timeCount === 1){
                end = new Date();
                t = end - start;
                log = floor(1000/t*timeCount);
                ctx.fillText("fps : " + floor(1000/t*timeCount) + "fps", 30, 90);
                start = new Date();
            }else{
                ctx.fillText("fps : " + log + "fps", 30, 90);
            }
            yield;
        }
    })();

    init();
    var start, end;
    setInterval(() => {
        step();
        draw();
        //time.next();
    }, 16);
    
    return { world : world, 
             drs : drs,
             drm : drm,
    };
};

var root = main();

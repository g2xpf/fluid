const PI = Math.PI;
const TWO_PI = 2 * PI;
var sqrt = (x, y) => Math.sqrt(x, y);
var rand = _ => Math.random();
var abs = x => Math.abs(x);
var cos = x => Math.cos(x);
var sin = x => Math.sin(x);
var min = (x, y) => Math.min(x, y);
var max = (x, y) => Math.max(x, y);
var floor = x => Math.floor(x);
var isZero = x => x == 0;
class Particle {
    constructor(x, y, isFluid){
        this.x = x;
        this.y = y;
        this.vx = 0;//rand() - 0.5;
        this.vy = 0;//rand() - 0.5;
        this.mass = 0.00020543;      // [kg]
        this.isStatic = isFluid;
        this.p = 0;     
        this.r = 0.007;  // 0.004 [m] as default 
        this.rho = 0;
        this.av = 0;
        this.inverseMass = this.mass == 0 ? 0 : 1 / this.mass;
	    this.inertia = 0.5 * this.r * this.r * this.mass;
	    this.inverseInertia = this.inertia == 0 ? 0 : 1 / this.inertia;
        this.meshX;
        this.meshY;
    }
};

class Wall {
    constructor(x, y, w, h, theta){
        this.x = x;
        this.y = y;
        this.w = w;
        this.h = h;
        this.a = theta;
        this.av = 0;
        this.vx = 0;
        this.vy = 0;
        this.av = 0;
        this.mass = 0;
        this.inverseMass = 0;
	    this.inertia = 0;
        this.inverseInertia = 0;
    }
};


var Ary = function(){
    var map = (f, array) => {
        for(var i = 0, len = array.length; i < len; ++i){
            f(array[i]);
        }
    };

    var add = (elem, array) => {
        array.push(elem);
    };

    return {
        map : map,
        add : add,
    };
}();


class CollisionInfo {
    constructor(){
        this.normalX;
        this.normalY;
        this.overlap = 1000;// max(canvas.height, canvas.width);
    }
}

class ContactConstraint {
    constructor(obj1, obj2, x, y, nx, ny, overlap){
        this.obj1 = obj1;
        this.obj2 = obj2;
        this.x = x;
        this.y = y;
        this.nx = nx; // normalized vector
        this.ny = ny;
        this.overlap = overlap;
        this.tx = -ny; // normalized normal vector of (nx, ny)
        this.ty = nx;
        this.r1x = x - obj1.x;
        this.r1y = y - obj1.y;
        this.r2x = x - obj2.x;
        this.r2y = y - obj2.y;

        this.massN = 0; // effective mass (vertical direction)
        this.massT = 0; // effective mass (normal direction)

        this.targetRelvN = 0; // target relative velocity (normal direction)
    }

   init (dt) {
        const cross1N = this.r1x * this.ny - this.r1y * this.nx;
	    const cross2N = this.r2x * this.ny - this.r2y * this.nx;
		const cross1T = this.r1x * this.ty - this.r1y * this.tx;
		const cross2T = this.r2x * this.ty - this.r2y * this.tx;
        
        this.massN = 1 / (this.obj1.inverseMass + this.obj2.inverseMass + cross1N * cross1N * this.obj1.inverseInertia + cross2N * cross2N * this.obj2.inverseInertia);
		this.massT = 1 / (this.obj1.inverseMass + this.obj2.inverseMass + cross1T * cross1T * this.obj1.inverseInertia + cross2T * cross2T * this.obj2.inverseInertia);
		
		// measure relative velocity
		const relvx = (this.obj1.vx - this.r1y * this.obj1.av) - (this.obj2.vx - this.r2y * this.obj2.av);
		const relvy = (this.obj1.vy + this.r1x * this.obj1.av) - (this.obj2.vy + this.r2x * this.obj2.av);
		const relvN = relvx * this.nx + relvy * this.ny;
		let e = 0.4; // restitution
		
		if (relvN > -1.0){//-0.5) {
			e = 0; // bodies are just "touching"
		}
		
		this.targetRelvN = -e * relvN;
		if (this.targetRelvN < 0) {
			this.targetRelvN = 0;
		}
		
		if (this.overlap > 0.05) {
			const separationVelocity = (this.overlap - 0.05) * 1.0 / dt;
			if (this.targetRelvN < separationVelocity) {
				this.targetRelvN = separationVelocity;
			}
		}
    }

    solve () {
        // measure relative velocity
		let relvx = (this.obj1.vx - this.r1y * this.obj1.av) - (this.obj2.vx - this.r2y * this.obj2.av);
		let relvy = (this.obj1.vy + this.r1x * this.obj1.av) - (this.obj2.vy + this.r2x * this.obj2.av);
		const relvN = relvx * this.nx + relvy * this.ny;
		
		// compute normal impulse
		let impN = (this.targetRelvN - relvN) * this.massN;
		if (impN < 0) {
			impN = 0;
		}
		
		// apply normal impulse
		this.obj1.vx += impN * this.nx * this.obj1.inverseMass;
		this.obj1.vy += impN * this.ny * this.obj1.inverseMass;
		this.obj1.av += impN * (this.r1x * this.ny - this.r1y * this.nx) * this.obj1.inverseInertia;
		this.obj2.vx -= impN * this.nx * this.obj2.inverseMass;
		this.obj2.vy -= impN * this.ny * this.obj2.inverseMass;
		this.obj2.av -= impN * (this.r2x * this.ny - this.r2y * this.nx) * this.obj2.inverseInertia;
		
		// measure relative velocity again
		relvx = (this.obj1.vx - this.r1y * this.obj1.av) - (this.obj2.vx - this.r2y * this.obj2.av);
		relvy = (this.obj1.vy + this.r1x * this.obj1.av) - (this.obj2.vy + this.r2x * this.obj2.av);
		const relvT = relvx * this.tx + relvy * this.ty;
		
		// compute tangent impulse
		let impT = (0 - relvT) * this.massT;
		
		// limit tangent impulse
		const mu = 0.43; // friction
		const maxTangentImpulse = impN * mu;
		if (impT > maxTangentImpulse) impT = maxTangentImpulse;
		else if (impT < -maxTangentImpulse) impT = -maxTangentImpulse;
		
		// apply tangent impulse
		/*this.obj1.vx += impT * this.tx * this.obj1.inverseMass;
		this.obj1.vy += impT * this.ty * this.obj1.inverseMass;
		this.obj1.av += impT * (this.r1x * this.ty - this.r1y * this.tx) * this.obj1.inverseInertia;
		this.obj2.vx -= impT * this.tx * this.obj2.inverseMass;
		this.obj2.vy -= impT * this.ty * this.obj2.inverseMass;
		this.obj2.av -= impT * (this.r2x * this.ty - this.r2y * this.tx) * this.obj2.inverseInertia;
        */

        this.obj1.av = 0;
        this.obj2.av = 0;
    }
}


class World {
    constructor(dt, ri, rho0){
        this.particles = [];
        this.walls = [];
        this.constraints = [];
        this.mesh = new Array(100);
        for(var i = 0; i < 100; ++i){
            this.mesh[i] = new Array(100);
        }
        for(var i = 0; i < 100; ++i){
            for(var j = 0; j < 100; ++j){
                this.mesh[i][j] = [];
            }
        }
        this.dt = dt;
        this.gravityX = 0.0;
        this.gravityY = 9.8;
        this.elecX = 0;
        this.elecY = 0;
        this.k = 0.6;       // gas constant
        // the radius of influence
        this.ri = ri;
        this.rho0 = rho0;
    }

    _meshInit(){
        for(var i = 0, len1 = this.mesh.length; i < len1; i++){
            for(var j = 0, len2 = this.mesh[i].length; j < len2; j++){
                this.mesh[i][j] = [];
            }
        }
    }

    _addToMesh(){
        Ary.map(particle => {
            var x = 49 + floor(particle.x / this.ri / 2);
            var y = 49 + floor(particle.y / this.ri / 2);
            if(0 <= x && x < 100 && 0 <= y && y < 100){
                particle.meshX = x;
                particle.meshY = y;
                this.mesh[x][y].push(particle);
            }
        }, this.particles);
    }

    _solveConstraints(){
        Ary.map(c => { c.init(this.dt) }, this.constraints);
        Ary.map(c => { c.solve(this.dt) }, this.constraints);
    }

    addWall(wall){
        this.walls.push(wall);
        console.log("Add a wall");
    };

    // collision to wall -------------------------------------

    _detect(){
        Ary.map(wall => {
            Ary.map(particle => {
                var dx = particle.x - wall.x;
                var dy = particle.y - wall.y;
                var r1 = particle.r;
                var r2 = sqrt((wall.w / 2) * (wall.w / 2) + (wall.h / 2) * (wall.h / 2));
                var r12 = sqrt(dx * dx + dy * dy);
                if(r12 <= r1 + r2){
                    var ci = new CollisionInfo();
                    if(this._checkCircleCuboidColl(particle, wall, ci)){
                        if(dx * ci.normalX + dy * ci.normalY < 0.0){
                            ci.normalX *= -1;
                            ci.normalY *= -1;
                        }
                        var normalX = ci.normalX;
                        var normalY = ci.normalY;
                        var worldX = particle.x - r1 * normalX;
                        var worldY = particle.y - r1 * normalY;
                        var overlap = ci.overlap;

                        this._addContact(new ContactConstraint(particle, wall, worldX, worldY, normalX, normalY, overlap));
                    }
                }
            }, this.particles);
        }, this.walls);
    };

    _addContact(contact){
        this.constraints.push(contact);
    }

    _checkCircleCuboidColl(particle, wall, ci){
        var theta = wall.a;
        var halfW = wall.w / 2;
        var halfH = wall.h / 2;
        // direction vector ( normalized )
        var nx = cos(theta);
        var ny = sin(theta);

        // normal vector ( normalized )
        var tx = -ny;
        var ty = nx;
        
        // vector directed from cuboid to circle
        var rx = particle.x - wall.x;
        var ry = particle.y - wall.y;

        // local vector of rx, ry
        var lx = rx * nx + ry * ny;
        var ly = rx * tx + ry * ty;
        
        // reset both vectors
        if(lx < 0){
            nx *= -1;
            ny *= -1;
        }

        if(ly < 0){
            tx *= -1;
            ty *= -1;
        }
        
        // crumped vector directed from cuboid to collision point 
        var crumpedX = (lx < 0 ? -1 : 1) * min(halfW, abs(lx));
        var crumpedY = (ly < 0 ? -1 : 1) * min(halfH, abs(ly));

        // restore vector from local to global
        var mx = crumpedX * cos(theta) - crumpedY * sin(theta);
        var my = crumpedX * sin(theta) + crumpedY * cos(theta);

        // vector directed from circle to collision point
        var x = rx - mx;
        var y = ry - my;
        
        var r = sqrt(x * x + y * y);
        var overlap = particle.r - r;

        if(overlap <= 0 || r == 0) return false;
        
        ci.overlap = overlap;
        ci.normalX = x / r;
        ci.normalY = y / r;

        return true;
    }

    // -----------------------------------------------------------------

    addParticle(part){
        console.log("Add an particle");
        Ary.add(part, this.particles);
    };

    _power(x, y){
        var ret = 1;
        for(var i = 0; i < y; ++i){
            ret *= x;
        }
        return ret;
    };

    _gradSpikyKernel(r, h){
        var alpha = -30 / (PI * this._power(h, 5)); 
        if(0 < r && r <= h){
            return alpha * this._power(h - r, 2) / r;
        }else{
            return 0;
        }
    }

    /*_viscosityKernel(r, h){
        var alpha = 10 / (3 * PI * h * h);
        if(0 < r && r <= h){
            return  alpha * (-this._power(r, 3) / (2 * this._power(h, 3)) + this._power(r, 2) / this._power(h, 2) + h / (2 * r) - 1);
        }else{
            return 0;
        }
    }

    _deltaViscosityKernel(r, h){
        var alpha = 10 / (3 * PI * h * h);
        if(0 < r && r <= h){
            return  alpha * ((-3) * this._power(r, 2) / (2 * this._power(h, 3)) + 2 * r / this._power(h, 2) - h / (2 * this._power(r, 2)) - 1);
        }else{
            return 0;
        }
    }*/

    _poly6Kernel(r, h){
        var alpha = 4 / (PI * this._power(h, 8));
        if(0 <= r && r <= h){
            return alpha * this._power((this._power(h, 2) - this._power(r, 2)), 3);
        }else{
            return 0;
        }
    }
    
    /*_gradViscosityKernel(r, h){
        var alpha = 10 / (3 * PI * this._power(h, 4));
        if(0 < r && r <= h){
            return alpha * (-3 * r / (2 * h) + 2 - this._power(h, 3) / (2 * this._power(r, 3)));
        }else{
            return 0;
        }
    }*/

    _laplacianViscosityKernel(r, h){
        var alpha = 20.0 / (3.0 * (PI * this._power(h, 5)));
        var ret;
        if(0 < r && r <= h){
            ret = alpha * (h - r);
        }else{
            ret = 0;
        }
        return ret;
    }

    _getPressure(rho){
        var gamma = 7;
        var c = 88.5;
        var ret;
        if(rho > this.rho0){
            ret = c * c * this.rho0 / gamma * (Math.pow(rho / this.rho0, gamma) - 1);
        }else{
            ret = 0;
        }

        return ret;
    }

    _getPressure2(rho){
        //; // constant value of gas
        return this.k * (rho - this.rho0);
    }

    step(){
        this._meshInit();
        this._addToMesh();
        this._solve();
        //this._confine();
        this.constraints = [];
        this._detect();
        this._solveConstraints();
        this._integrate();
    };


    _confine(){
        var w = 0.07, h = 0.05, k = 100.9;
        Ary.map(particle => {
            var x = particle.x;
            var y = particle.y;
            if(x <= -w) particle.vx += k * (-w - x);
            if(x >= w) particle.vx -= k * (x - w);
            if(y <= -h) particle.vy += k * (-h - y);
            if(y >= h) particle.vy -= k *(y - h);
        }, this.particles);
    }

    _rhoInit(){
        Ary.map(particle => {
            particle.rho = 0;
        }, this.particles);  
    };


    _solve(){
        this._rhoInit();
        var len = this.particles.length;
        var mu = 0.7;
        for(var i = 0; i < len; ++i){
            var p1 = this.particles[i];
            var x = p1.meshX;
            var y = p1.meshY;
            for(var dx = -1; dx <= 1; ++dx){
                for(var dy = -1; dy <= 1; ++dy){
                    var nx = x + dx; 
                    var ny = y + dy; 
                    if(!(0 <= nx && nx < 100 && 0 <= ny && ny < 100)) continue;
                    Ary.map( p2 => {
                        var r = sqrt(this._power(p2.x - p1.x, 2) + this._power(p2.y - p1.y, 2));
                        var k = this._poly6Kernel(r, this.ri);
                        p1.rho += p2.mass * k;
                    }, this.mesh[nx][ny]); 
                }
            }
            var rho = p1.rho;
            var p = this._getPressure2(rho);
            p1.p = p;
        }

        for(var i = 0; i < len; ++i){
            var p1 = this.particles[i];
            p1.vx += this.gravityX * this.dt / 9;// * (this.dt / 0.016);
            p1.vy += this.gravityY * this.dt / 9;// * (this.dt / 0.016);
            if(p1.isStatic) continue;
            var x = p1.meshX;
            var y = p1.meshY;
            for(var dx = -1; dx <= 1; ++dx){
                for(var dy = -1; dy <= 1; ++dy){
                    var nx = x + dx; 
                    var ny = y + dy; 
                    if(!(0 <= nx && nx < 100 && 0 <= ny && ny < 100)) continue;
                    Ary.map(p2 => {
                        //if(i === j) continue;
                        var dx = p2.x - p1.x;
                        var dy = p2.y - p1.y;
                        var r = sqrt(this._power(dx, 2) + this._power(dy, 2));

                        // fp : pressure
                        // fv : viscosity

                        var gradP = this._gradSpikyKernel(r, this.ri);
                        //var fpx = (-p2.mass * (p1.p / (p1.rho * p1.rho) + p2.p / (p2.rho * p2.rho)) * gradP) * dx;
                        var fpx = ((p1.p + p2.p) / (2 * p2.rho)) * gradP * dx / p1.rho;
                        //var fpy = (-p2.mass * (p1.p / (p1.rho * p1.rho) + p2.p / (p2.rho * p2.rho)) * gradP) * dy;
                        var fpy = ((p1.p + p2.p) / (2 * p2.rho)) * gradP * dy / p1.rho;
                        var gradV = this._laplacianViscosityKernel(r, this.ri);
                        var fvx = mu * (p2.vx - p1.vx) / p2.rho * gradV * dx / p1.rho*1.3;
                        var fvy = mu * (p2.vy - p1.vy) / p2.rho * gradV * dy / p1.rho*1.3;
    
                        fpx *= 0.4;
                        fpy *= 0.4;
                        fvx *= 0.3;
                        fvy *= 0.3;

                        p1.vx += p2.mass * (fpx + fvx) * this.dt;
                        p1.vy += p2.mass * (fpy + fvy) * this.dt;
                    }, this.mesh[nx][ny]);
                }
            }
        }
    }

    _integrate(){
        //console.log(this.elecX, this.elecY);
        Ary.map(particle => {
            //if(!particle.isStatic){
                particle.vx += this.elecX;
                particle.vy += this.elecY;
                particle.x += particle.vx * this.dt;
                particle.y += particle.vy * this.dt;
            //}
        }, this.particles);
    }
}

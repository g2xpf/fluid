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
    constructor(x, y, z, isFluid){
        this.x = x;
        this.y = y;
        this.z = z;
        this.vx = 0;//rand() - 0.5;
        this.vy = 0;//rand() - 0.5;
        this.vz = 0;
        this.mass = 0.00020543;      // [kg]
        this.isStatic = isFluid;
        this.p = 0;
        this.r = 0.004;  // 0.004 [m] as default 
        this.rho = 0;
        this.av = 0;
        this.fx = 0;
        this.fy = 0;
        this.fz = 0;
        this.inverseMass = this.mass == 0 ? 0 : 1 / this.mass * 10000000;
	    this.inertia = 0.5 * this.r * this.r * this.mass * 10000000;
	    this.inverseInertia = this.inertia == 0 ? 0 : 1 / this.inertia;
        this.meshX;
        this.meshY;
        this.meshZ;
    }
};

class Wall {
    constructor(x, y, z, w, h, theta){
        this.x = x;
        this.y = y;
        this.z = z;
        this.w = w;
        this.h = h;
        this.a = theta;
        this.av = 0;
        this.vx = 0;
        this.vy = 0;
        this.vz = 0;
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
        this.baseOverlap = 0.00005;
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
		let e = 0.2; // restitution
		
		if (relvN > -1.0){//-0.5) {
			e = 0; // bodies are just "touching"
		}
		
		this.targetRelvN = -e * relvN;
		if (this.targetRelvN < 0) {
			this.targetRelvN = 0;
		}
		
		if (this.overlap > this.baseOverlap) {
			const separationVelocity = (this.overlap - this.baseOverlap) * 1.0 / dt;
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
		var dy = impN * this.ny * this.obj1.inverseMass; 
        this.obj1.vy += dy;
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
		this.obj1.vx += impT * this.tx * this.obj1.inverseMass;
		this.obj1.vy += impT * this.ty * this.obj1.inverseMass;
		this.obj1.av += impT * (this.r1x * this.ty - this.r1y * this.tx) * this.obj1.inverseInertia;
		this.obj2.vx -= impT * this.tx * this.obj2.inverseMass;
		this.obj2.vy -= impT * this.ty * this.obj2.inverseMass;
		this.obj2.av -= impT * (this.r2x * this.ty - this.r2y * this.tx) * this.obj2.inverseInertia;
        

        this.obj1.av = 0;
        this.obj2.av = 0;
    }
}


class World {
    constructor(dt, ri, rho0){
        this.particles = [];
        this.walls = [];
        this.constraints = [];
        // initialize meshes
        this.mesh;
        this.dt = dt;
        this.gravityX = 0.0;
        this.gravityY = 9.8;
        this.gravityZ = 0.0;
        this.elecX = 0;
        this.elecY = 0;
        this.elecZ = 0;
        this.k = 1.0;       // gas constant
        this.mu = 0.2;      // viscosity constant
        // the radius of influence
        this.scale = 0.004;
        this.d = Math.pow(0.00020543 / 600, 1/3.0);
        this.ri = ri;
        this.rho0 = rho0;
        this.meshNum = 30;
    }

    makeMesh(){
        this.mesh = new Array(this.meshNum);
        for(var i = 0; i < this.meshNum; ++i){
            this.mesh[i] = new Array(this.meshNum);
            for(var j = 0; j < this.meshNum; ++j){
                this.mesh[i][j] = new Array(this.meshNum);
                for(var k = 0; k < this.meshNum; ++k){
                    this.mesh[i][j][k] = [];
                }    
            }
        }
    }

    _meshInit(){
        for(var i = 0, len1 = this.mesh.length; i < len1; i++){
            for(var j = 0, len2 = this.mesh[i].length; j < len2; j++){
                for(var k = 0, len3 = this.mesh[i][j].length; k < len3; ++k){
                    this.mesh[i][j][k] = [];
                }
            }
        }
    }

    _addToMesh(){
        Ary.map(particle => {
            var x = this.meshNum / 2 + floor(particle.x / this.ri / 2) + 1;
            var y = this.meshNum / 2 + floor(particle.y / this.ri / 2) + 1;
            var z = this.meshNum / 2 + floor(particle.z / this.ri / 2) + 1;
            if(0 <= x && x < this.meshNum && 0 <= y && y < this.meshNum && 0 <= z && z < this.meshNum){
                particle.meshX = x;
                particle.meshY = y;
                particle.meshZ = z;
                this.mesh[x][y][z].push(particle);
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
    
    _power(x, y){
        var ret = 1;
        for(var i = 0; i < y; ++i){
            ret *= x;
        }
        return ret;
    };

    addParticle(part){
        console.log("Add an particle");
        Ary.add(part, this.particles);
    };

    _poly6Kernel(r, h){
        var alpha = 315 / (64 * PI * this._power(h, 9));
        var ret = alpha * this._power((this._power(h, 2) - this._power(r, 2)), 3);
        return ret;
    }

    _gradSpikyKernel(r, h){
        var alpha = -45 / (PI * this._power(h, 6));
        if(0 < r && r <= h){
            return alpha * this._power(h - r, 2) / r;
        }else{
            return 0;
        }
    }

    _laplacianViscosityKernel(r, h){
        var alpha = 45.0 / (PI * this._power(h, 6));
        return alpha * (h - r);
    }

    _getPressure(rho){
        var gamma = 7;
        var c = 0.5;
        return c * c * this.rho0 / gamma * (Math.pow(rho / this.rho0, gamma) - 1);

    }

    _getPressure2(rho){
        //; // constant value of gas
        return this.k * (rho - this.rho0);
    }

    step(){
        this._meshInit();
        this._addToMesh();
        this._solve();
        this.constraints = [];
        this._detect();
        this._solveConstraints();
        this._integrate();
    };

    _rhoInit(){
        Ary.map(particle => {
            particle.rho = 0;
        }, this.particles);  
    };

    _solve(){
        this._rhoInit();
        var len = this.particles.length;
        for(var i = 0; i < len; ++i){
            var p1 = this.particles[i];
            var x = p1.meshX;
            var y = p1.meshY;
            var z = p1.meshZ;
            var sum = 0.0;
            for(var dz = -1; dz <= 1; ++dz){
                for(var dy = -1; dy <= 1; ++dy){
                    for(var dx = -1; dx <= 1; ++dx){
                        var nx = x + dx; 
                        var ny = y + dy; 
                        var nz = z + dz;
                        if(!(0 <= nx && nx < this.meshNum && 0 <= ny && ny < this.meshNum && 0 <= nz && nz < this.meshNum)) continue;
                        Ary.map( p2 => {
                            if(p1 != p2){
                                var r = sqrt(this._power(p2.x - p1.x, 2) + this._power(p2.y - p1.y, 2) + this._power(p2.z - p1.z, 2));
                                if(r < this.ri){
                                    var k = p2.mass * this._poly6Kernel(r, this.ri);
                                    sum += k;
                                }
                            }
                        }, this.mesh[nx][ny][nz]); 
                    }
                }
            }
            p1.rho += sum;
            var rho = p1.rho;
            var p = this._getPressure2(rho);
            p1.p = p;
        }
        for(var i = 0; i < len; ++i){
            var p1 = this.particles[i];
            if(p1.isStatic) continue;
            p1.vx += this.elecX;
            p1.vy += this.elecY;
            p1.vz += this.elecZ;

            p1.vx += this.gravityX * this.dt;
            p1.vy += this.gravityY * this.dt;
            p1.vz += this.gravityZ * this.dt;
            var x = p1.meshX;
            var y = p1.meshY;
            var z = p1.meshZ;
            for(var dz = -1; dz <= 1; ++dz){
                for(var dy = -1; dy <= 1; ++dy){
                    for(var dx = -1; dx <= 1; ++dx){
                        var nx = x + dx; 
                        var ny = y + dy; 
                        var nz = z + dz;
                        if(!(0 <= nx && nx < this.meshNum && 0 <= ny && ny < this.meshNum && 0 <= nz && nz < this.meshNum)) continue;
                        Ary.map(p2 => {
                            //if(i === j) continue;
                            var dx = p2.x - p1.x;
                            var dy = p2.y - p1.y;
                            var dz = p2.z - p1.z;
                            var r = sqrt(this._power(dx, 2) + this._power(dy, 2) + this._power(dz, 2));
                            if(0 < r && r < this.ri){
                                // fp : pressure
                                // fv : viscosity

                                var gradP = this._gradSpikyKernel(r, this.ri);
                                //var fpx = (-p2.mass * (p1.p / (p1.rho * p1.rho) + p2.p / (p2.rho * p2.rho)) * gradP) * dx;
                                //var fpy = (-p2.mass * (p1.p / (p1.rho * p1.rho) + p2.p / (p2.rho * p2.rho)) * gradP) * dy;
                                var fpx = 0.5*(p1.p + p2.p) * gradP * dx / (p1.rho * p2.rho);
                                var fpy = 0.5*(p1.p + p2.p) * gradP * dy / (p1.rho * p2.rho);
                                var fpz = 0.5*(p1.p + p2.p) * gradP * dz / (p1.rho * p2.rho);
                                var gradV = this._laplacianViscosityKernel(r, this.ri);
                                var fvx = this.mu * (p2.vx - p1.vx) * gradV / (p1.rho * p2.rho);
                                var fvy = this.mu * (p2.vy - p1.vy) * gradV / (p1.rho * p2.rho);
                                var fvz = this.mu * (p2.vz - p1.vz) * gradV / (p1.rho * p2.rho);

                                if(fvx !== fvx || fvy !== fvy || fvz !== fvz){
                                    console.error("### PARTICLE GOES OUT OF MESHES' RANGE ###");
                                }

                                p1.vx += p2.mass * (fpx + fvx) * this.dt;
                                p1.vy += p2.mass * (fpy + fvy) * this.dt;
                                p1.vz += p2.mass * (fpz + fvz) * this.dt;
                                if(abs(p1.vx) > 100 || abs(p1.vy) > 100 || abs(p1.vz > 100)){
                                    console.log("hoge");
                                }
                            }
                        }, this.mesh[nx][ny][nz]);
                    }
                }
            }
        }
    }

    _integrate(){
        //console.log(this.elecX, this.elecY);
        Ary.map(particle => {
            //if(!particle.isStatic){
            particle.x += particle.vx * this.dt;
            particle.y += particle.vy * this.dt;
            particle.z += particle.vz * this.dt;

            // only 2D oparation
            particle.vz = 0;
            particle.z = 0;
            //}
        }, this.particles);
    }
}

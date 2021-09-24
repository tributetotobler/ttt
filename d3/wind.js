// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20), container="canvas"
//
// r2d3: https://rstudio.github.io/r2d3
//

let context = canvas.node().getContext("2d");
const margin = 30;
const fadeFillStyle = "rgba(0, 0, 0, 0.95)";
const lifespan = data.params.lifespan;
const scalefact = data.params.scalefact;
const linewidth = data.params.linewidth;
const nbparticules = data.params.nbparticules;
const resolution = data.params.resolution;

toRows = function(data){
  let keys = Object.keys(data);
  console.log(keys);
  let N = data[keys[1]].length;
  let dataR = [];
  for (let i=0;i<N;i++){
    let nd ={};
    for (var k=0; k< keys.length;k++){
      nd[keys[k]]=data[keys[k]][i];
    }
    dataR.push(nd);
  }
  return dataR;
}

data = toRows(data.data)

const L = d3.max(data,d => d.x)-d3.min(data,d => d.x);
const H = d3.max(data,d => d.y)-d3.min(data,d => d.y);
const mx = d3.min(data,d => d.x);
const my = d3.min(data,d => d.y);
let x = v => margin+(v-mx)/H*(height-2*margin);
let y = v => height-((v-my)/H*(height-2*margin)+margin);



data.forEach(function(cell){
  cell.i=Math.round((cell.x-mx)/resolution);
  cell.j=Math.round((cell.y-my)/resolution);
});

console.log(JSON.stringify(data[0]))

let ijmap = d3.nest().key(cell => cell.i).key(cell => cell.j).rollup(cells => cells[0]).map(data);
let ikeys = Object.keys(ijmap);
get_grad = function(x,y){
  ip = Math.round((x-mx)/resolution);
  jp = Math.round((y-my)/resolution);
  
  dx=0;
  dy=0;
  
  if(ijmap["$"+ip] && ijmap["$"+ip]["$"+jp]){
    cell = ijmap["$"+ip]["$"+jp];
    dx=cell.dx;
    dy=cell.dy;
  }
  
  return {dx:dx*scalefact,dy:dy*scalefact} ;
};

sample_pos=function(){
  let i = ikeys[Math.floor(Math.random()*ikeys.length)];
  let jkeys = Object.keys(ijmap[i]);
  let j = jkeys[Math.floor(Math.random()*jkeys.length)];
  return {x:ijmap[i][j].x,y:ijmap[i][j].y};
};

const particles = [];
for (var i=0;i<nbparticules;i++){
  par = sample_pos();
  par.dx=0;
  par.dy=0;
  par.nbc=Math.floor(Math.random()*lifespan);
  particles.push(par);
}


context.canvas.style.background = "#ffffff";
context.lineWidth = linewidth;
context.fillStyle = fadeFillStyle;
context.strokeStyle = "#66a61e";
context.beginPath();
context.rect(0,0, width, height);
context.fillStyle = "white";
context.fill();

const nb_an =5000;

var update = function () {
    var prev = context.globalCompositeOperation;
    context.globalCompositeOperation = "destination-in";
    context.fillRect(0,0, width, height);
    context.globalCompositeOperation = prev;
    for (const p of particles) {
      context.beginPath();
      context.moveTo(x(p.x), y(p.y));
      p.x += p.dx;
      p.y += p.dy;
      context.lineTo(x(p.x), y(p.y));
      context.stroke();
      grad = get_grad(p.x,p.y);
      p.dx = grad.dx;
      p.dy = grad.dy;
      p.nbc++;
      if(p.nbc>=lifespan){
        newpos = sample_pos();
        p.x=newpos.x;
        p.y=newpos.y;
        grad = get_grad(p.x,p.y);
        p.dx = grad.dx;
        p.dy = grad.dy;
        p.nbc=0
      }
    }
    i++;
    requestAnimationFrame(update);
};
requestAnimationFrame(update);
  
  




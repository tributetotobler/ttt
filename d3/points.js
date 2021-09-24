// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20), container="canvas"
//
// r2d3: https://rstudio.github.io/r2d3
//

let context = canvas.node().getContext("2d");
console.log(canvas.node());
const n = 200;
const margin = 60;
const fadeFillStyle = "rgba(0, 0, 0, 0.95)";
const lifespan = 40;
const PARTICLE_LINE_WIDTH = 2;
const NBPART = 2500;
const L = d3.max(data,d => d.x)-d3.min(data,d => d.x);
const H = d3.max(data,d => d.y)-d3.min(data,d => d.y);
const mx = d3.min(data,d => d.x);
const my = d3.min(data,d => d.y);
const resolution = data[0].resolution;
let x = v => margin+(v-mx)/H*(height-2*margin);
let y = v => height-((v-my)/H*(height-2*margin)+margin);
data.forEach(function(cell){cell.i=(+cell.x-mx)/resolution;cell.j=(+cell.y-my)/resolution});
let ijmap = d3.nest().key(cell => cell.i).key(cell => cell.j).rollup(cells => cells[0]).map(data);
let ikeys = Object.keys(ijmap);
get_ij=function(){
  return ijmap
}
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
  return {dx:dx*50,dy:dy*50} ;
};
sample_pos=function(){
  let i = ikeys[Math.floor(Math.random()*ikeys.length)];
  let jkeys = Object.keys(ijmap[i]);
  let j = jkeys[Math.floor(Math.random()*jkeys.length)];
  return {x:ijmap[i][j].x,y:ijmap[i][j].y};
};
context.canvas.style.background = "#fff";
context.lineWidth = PARTICLE_LINE_WIDTH;
for (let id=0;id<5000;id++){
      context.beginPath();
      //console.log(JSON.stringify(data[id]))
      //context.moveTo(x(data[id].x), y(data[id].y));
      //context.lineTo(x(data[id].x+50*data[id].dx), y(data[id].y+50*data[id].dy));
      p=sample_pos()
      grad=get_grad(p.x,p.y)
      context.moveTo(x(p.x), y(p.y));
      context.lineTo(x(p.x+grad.dx), y(p.y+grad.dy));
      context.stroke();
}
  




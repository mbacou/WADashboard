// !preview r2d3 data=NULL

var pal = ['#3C8DBC','#DD4B39','#00A65A','#00C0EF','#F39C12','#0073B7',
  '#001F3F','#39CCCC','#3D9970','#01FF70','#FF851B','#F012BE','#605CA8',
  '#D81B60','#111111','#D2D6DE'];

// Interactions
function handleMouseOver(d, i) {
   d3.select(this)
   .attr("fill-opacity", 0.5);
}

function handleMouseOut(d, i) {
  d3.select(this)
  .attr("fill-opacity", 1);
}

function handleDataMouseOver() {
  d3.select(this)
  .raise()
  .attr("font-size", font_size*3);
}

function handleDataMouseOut() {
  d3.select(this)
  .attr("font-size", font_size);
}


const obj = svg
  .insert("svg:g")
  .attr("class", "sheet_1");

obj.selectAll("svg").remove();

// Init external design
d3.xml("./svg/sheet_1_edited.svg")
  .then(d => {
  obj.node().append(d.documentElement);

  obj
    .selectAll("rect").merge(obj.selectAll("path"))
      .on("mouseover", handleMouseOver)
      .on("mouseout", handleMouseOut)
      .on("click", function() {
        Shiny.setInputValue(
          "bar_clicked", {
            "id" : d3.select(this).attr("id"),
            "value" : d3.select(this).attr("value"),
            "var" : d3.select(this).attr("var"),
            "color" : d3.select(this).attr("fill")
            }, {priority: "event"}
          );
        //console.log(d3.select(this).attr("d"));
      });

  obj
    .selectAll("text[class=data]")
      .on("mouseover", handleDataMouseOver)
      .on("mouseout", handleDataMouseOut);

});


// Rendering
r2d3.onRender(function(data, svg, width, height, options) {

var root = svg.select(".sheet_1").select("svg");

  root
    .selectAll("rect")
    .data(data)
    //.attr("height", d => 1000*d.value)
    .attr("var", d => d.id)
    .attr("value", d => d.value);

  root
    .selectAll("text[class=data]")
    .data(data)
    // will need to include data mapping here
    .attr("text-anchor", "middle")
    .text(d => d3.format("(.2f")(d.value));

});


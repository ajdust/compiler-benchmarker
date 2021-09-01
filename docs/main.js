// https://github.com/plotly/plotly.js/issues/65
function legendHover(my) {
    const legends = my.querySelectorAll(".legendtoggle");

    for (const legend of legends) {
        legend.onmouseenter = (d) => {
            const traceName = d.target.previousSibling.previousSibling.getAttribute("data-unformatted");
            const cn = my.data.map(v => v.name).indexOf(traceName);
            if (cn < 0) return;

            const curve = my.data[cn].x;
            Plotly.Fx.hover("plot",
                curve.map((_, i) => { return { curveNumber: cn, pointNumber: i }; })
            );
        };

        legend.onmouseleave = () => {
            Plotly.Fx.hover("plot", []);
        };
    }
}

document.addEventListener("DOMContentLoaded", async function() {

	const colors = await (await fetch("language_colors.json")).json();
    const csv = await (await fetch("r.csv")).text();
    const results = Papa.parse(csv).data.slice(1);

    const layout = {
        title: "Compiler Benchmarks",
        showLegend: true,
        height: 1000,
        width: 1000,
        xaxis: {
            title: {
                text: "Number of Functions"
            }
        },
        yaxis: {
            title: {
                text: "Elapsed (sec)"
            }
        }
    };

    const data = [];
    const byName = {};

    for (let result of results) {
    	const [numFun, lang, comp, args, _, sec, mem, err] = result;
    	const name = `${lang} ${comp} ${args}`;

    	let series = byName[name];
    	if (!series) {
    		series = {
    			mode: "lines+markers",
    			name: name,
    			x: [], y: [], text: [], meta: [],
    			marker: {
    				size: [],
    				line: { width: 2 },
    				symbol: []
    			}
    		};
			byName[name] = series;
    		data.push(series);
    	}

    	series.x.push(parseFloat(numFun));
    	series.y.push(parseFloat(sec));
    	series.text.push(`${name}: ${numFun} functions, ${sec} seconds, memory: ${mem} KB`);
    	series.meta.push({ compiler: comp });
    	series.marker.color = colors[lang];
    	series.marker.line.color = colors[lang];
    	series.marker.size.push(Math.max(5, mem / 5e4));
    	series.marker.symbol.push(
    		args.indexOf("-O") >= 0
    		|| args.indexOf("release") >= 0
    		|| args.indexOf("opt-level") >= 0
    		|| args.indexOf("opt-inline") >= 0
    		? "diamond" : "circle"); // diamonds are optimized build, circle unoptimized

    }

    console.log(data);
    Plotly.newPlot("plot", data, layout).then(legendHover);
});
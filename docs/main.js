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
    const csv = await (await fetch("202109041755_results.csv")).text();
    const results = Papa.parse(csv).data.slice(1);

    const layout = {
        title: "Compiler Benchmarks",
        showLegend: true,
        height: window.innerHeight > 1000 ? window.innerHeight / 1.2 : window.innerHeight,
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
    	if (err || sec === "")
    		continue;

    	const name = `${lang} ${comp} ${args}`;
    	const hasOptimizeFlag = args.indexOf("-O") >= 0
    		|| args.indexOf("release") >= 0
    		|| args.indexOf("opt-level") >= 0
    		|| args.indexOf("opt-inline") >= 0;
    	const isJIT = lang === "Java" || lang === "Kotlin" || lang === "Scala" || lang === "FSharp" || lang === "CSharp";
    	const isNative = !isJIT;

    	let series = byName[name];
    	if (!series) {
    		series = {
    			mode: "lines+markers",
    			name: name,
    			x: [], y: [], text: [], meta: [],
    			meta: {
    				compiler: comp,
    				hasOptimizeFlag: hasOptimizeFlag,
    				isNative: isNative,
    				isJIT: isJIT,
    			},
    			marker: {
    				size: [],
    				line: { width: 2 },
    				symbol: []
    			},
    		};
			byName[name] = series;
    		data.push(series);
    	}

    	series.x.push(parseFloat(numFun));
    	series.y.push(parseFloat(sec));
    	series.text.push(`${name}: ${numFun} functions, ${sec} seconds, memory: ${(mem/1e3).toFixed(2)} MB`);
    	series.marker.color = colors[lang];
    	series.marker.line.color = colors[lang];
    	series.marker.size.push(Math.max(5, mem / 5e4));
    	series.marker.symbol.push(hasOptimizeFlag ? "diamond" : "circle");
    }

    console.log(data);
    const plot = document.getElementById("plot");
    const checkOpt = document.getElementById("toggle-opt");
    const checkNoOpt = document.getElementById("toggle-no-opt");
    const checkNative = document.getElementById("toggle-native");
    const checkJIT = document.getElementById("toggle-jit");
    function updateChecks() {
		checkOpt.checked = false;
		checkNoOpt.checked = false;
		checkNative.checked = false;
		checkJIT.checked = false;
		for (let i = 0; i < plot.data.length; ++i) {
			const el = plot.data[i];
			if (el.visible === false || el.visible === "legendonly")
				continue;
			if (el.meta.hasOptimizeFlag)
				checkOpt.checked = true;
			if (!el.meta.hasOptimizeFlag)
				checkNoOpt.checked = true;
			if (el.meta.isNative)
				checkNative.checked = true;
			if (el.meta.isJIT)
				checkJIT.checked = true;
		}
    }

    Plotly.newPlot(plot, data, layout).then(legendHover);

    /* events */
    plot.on("plotly_restyle", (change) => {
    	if (change.length === 2 && change[0].visible && change[1].length === 1 && typeof change[1][0] === "number") {
    		console.log(`Changed visible to ${change[0].visible} on:`, plot.data[change[1][0]]);
    		// loop through the data, see if all of each category are off - disable the checkbox when all are off
    		updateChecks();
    	}
	});

	checkOpt.addEventListener("change", (event) => {
		const ck = event.currentTarget.checked;
		console.log('opt', ck);
		const idx = plot.data.map((v, i) => [i, v]).filter(e => e[1].meta.hasOptimizeFlag).map(v => v[0]);
		Plotly.restyle(plot, { visible: ck ? true : "legendonly" }, idx)
		updateChecks();
	});

	checkNoOpt.addEventListener("change", (event) => {
		const ck = event.currentTarget.checked;
		console.log('no-opt', ck);
		const idx = plot.data.map((v, i) => [i, v]).filter(e => !e[1].meta.hasOptimizeFlag).map(v => v[0]);
		Plotly.restyle(plot, { visible: ck ? true : "legendonly" }, idx)
		updateChecks();
	});

	checkNative.addEventListener("change", (event) => {
		const ck = event.currentTarget.checked;
		console.log('native', ck);
		const idx = plot.data.map((v, i) => [i, v]).filter(e => e[1].meta.isNative).map(v => v[0]);
		Plotly.restyle(plot, { visible: ck ? true : "legendonly" }, idx)
		updateChecks();
	});

	checkJIT.addEventListener("change", (event) => {
		const ck = event.currentTarget.checked;
		console.log('jit', ck);
		const idx = plot.data.map((v, i) => [i, v]).filter(e => e[1].meta.isJIT).map(v => v[0]);
		Plotly.restyle(plot, { visible: ck ? true : "legendonly" }, idx)
		updateChecks();
	});

	updateChecks();
});
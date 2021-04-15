var getSolc = (function() {
    var solc;
    return function() {
        if (solc) {
            return  solc;
        }

        window.BrowserSolc.getVersions(function(soljsonSources, soljsonReleases) {
            var solcVersion = soljsonReleases[_.keys(soljsonReleases)[0]];
            window.BrowserSolc.loadVersion(solcVersion, function(c) {
              solc = c;
            });
        });
        return solc;
    };
})();
getSolc();


// Compile a single Yul contracts to a bytecode contract.
export function compileContract(name, yulSrc) {
    let solc = getSolc();
    let yulSrcNormalized = yulSrc.replace(/\\"/g, '"');
    let input = {
        language: "Yul",
        sources: { "dummy.yul": {
            content: yulSrcNormalized
          }
        },
        settings: {
          outputSelection: { "*": { "*": ["*"], "": [ "*" ] } }
        }
    };
    let output = solc.compile(JSON.stringify(input));
    return output["contracts"]["dummy.yul"][name]["evm"]["bytecode"]["object"];
}

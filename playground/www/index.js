import * as fe from "playground";

document.getElementById("run").onclick = function() {
    document.getElementById("result").value = "";

    const code = document.getElementById("code").value;

    let namedCompiledContracts;
    var runResult = {};
    try {
        namedCompiledContracts = fe.compile(code);
        for (const name of Object.keys(namedCompiledContracts)) {
            let contract = namedCompiledContracts[name];
            let abi = JSON.parse(contract.json_abi);
            if (!Object.values(abi).some(f => f.name === "__main__"))
            {
                continue;

            }
            try {
                runResult[name] = fe.run(namedCompiledContracts, name, "__main__", [], [])
            } catch(e) {
                runResult[name] = e
            }
        }
    }
    catch(e) {
        document.getElementById("result").value = e;
        return;
    }

    let txtArea = document.getElementById("result");
    for (const [name, res] of Object.entries(runResult)) {
        txtArea.value += name + ': ' + res + '\r\n';
    }
}

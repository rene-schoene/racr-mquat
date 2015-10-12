# GLPK-ILP-Generator

org.haec.optimizer.ilp.GLPK_ILPGenerator

## Generation of ILP

- Times: `/opt/luna/eclipse/logs/durations.csv`, times in milli-sec

## Solving of ILP

- Using GLPK
- Times: `/home/rschoene/git/mquat/project_systems/gen_sys/*/sol.csv`, times in nano-sec
- ILP: `/home/rschoene/git/mquat/project_systems/gen_sys/*/gip.glpk`
- Solution: `/opt/luna/eclipse/logs/csvs/<timestamp>.csv`

# Toy-Generator

org.coolsoftware.requests.resource.request.ui.actions.ILPGenerator2

## Generation of ILP

- Times: `/home/rschoene/git/mquat/project_systems/gen_sys/*/gen.csv`, times in milli-sec

## Solving of ILP

- Using LP_solve
- Times: `/home/rschoene/git/mquat/project_systems/gen_sys/*/sol.csv`, times in milli-sec
- Solution: `/home/rschoene/git/mquat/project_systems/gen_sys/*/ilp-result-max-3.txt`

# LP-Generator

org.haec.optimizer.ilp.ILPGenerator

## Generation of ILP

- Times: `/home/rschoene/git/mquat/project_systems/gen_sys/*/gen.csv`, times in milli-sec

## Solving of ILP

- Using LP_solve
- Times: `/home/rschoene/git/mquat/project_systems/gen_sys/*/sol.csv`, times in milli-sec

# Schemas

- `durations.csv`: problemName, timestamp¹, dSetup, dBinVar, dUsage, dProperty, dBuffer, dTotal
- `*/gen.csv`: solver, timestamp², architectural constraints, resource negotation, nfp negotiation, objective function
- `*/sol.csv`: solver, timestamp², rows, cols, duration

¹: "DD.MM.YY HH:MM"

²: "YYYY-MM-DD'T'HH:MM:SS.NNN" (Python datetime.datetime.isoformat())

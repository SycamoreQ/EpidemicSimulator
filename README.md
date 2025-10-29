This project implements a toy metapopulation epidemic simulator with a DDQN training loop, supports parameter calibration over infection rate beta and travel scaling, logs experiments to W&B, renders a loss‑surface heatmap with Python/Seaborn, and optionally monitors results in near real‑time using Spark Structured Streaming.
Goals
* Calibrate beta and travel to minimize a loss that combines arrival time and peak infections across countries. 
* Train DDQN on the calibrated configuration while logging key metrics and artifacts to a W&B project. 
* Visualize the calibration surface as a heatmap and optionally attach it to the same W&B run for quick inspection. 
* Stream snapshots from training into a micro‑batch pipeline for rolling analytics and fault‑tolerant archiving. 
Core components
* Environment and dynamics: integer‑conserving local updates split susceptible and infected counts via proportional apportionment (largest‑remainder method), avoiding drift from independent rounding. 
* Mobility: integer‑conserving allocation across the connection matrix with world‑level conservation checks, ensuring cross‑border flows preserve global mass each step. 
* Calibration: a grid surface over beta×travel runs short deterministic episodes to compute loss per configuration and selects the minimum for training. 
* Training: DDQN training with periodic evaluation and logging; longer runs use the calibrated pair for stability. 

Real‑time analytics (streaming)
* Export compact per‑epoch or per‑eval snapshots (country, t, i, arrival, peakI, loss) to stream_in/snapshots as training progresses. 
* Start a Structured Streaming query that reads new snapshots with readStream, computes rolling aggregates, and writes to Parquet/CSV using foreachBatch with a checkpoint for fault tolerance. 
* Choose output modes (append/update/complete) per your aggregation semantics, and prefer foreachBatch to reuse normal batch writers and implement idempotence via batchId when necessary. 
W&B tracking
* Initialize a run for each calibration or training execution and log metrics, configuration, and artifacts for experiment management and comparison. 
	•	Use Sweeps to define a search space for beta, travel, and DDQN hyperparameters and launch a controller to orchestrate multiple agents locally or remotely. 


How to run 
* Before running the code , set the WANDB_API_KEY like "export WANDB_API_KEY=..." in the project root
* To run the code , set it to the project home directory and enter "sbt run" . This will prompt the user to enter either 1 or 2 which is running the EpidemicSimulator and EpidemicStreamer respectively.
  The EpidemicStreamer is currently being developed so to run the code , press 1 
* A few warnings from spark will appear and then the WANDB initialization message will also appear which means that WANDB is initialized and the user can see real time graph analytics in the WANDB dashboard.
* Upon finishing epochs , the model outputs the results i.e beta and travel scores in two forms: csv and parquet as results.csv and results.parquet respectively. The WANDB logs are also saved.

package epidemic

final case class HyperParams(
                              gamma: Double = 0.97,
                              lr: Double = 5e-4,
                              epsilonStart: Double = 1.0,
                              epsilonEnd: Double = 0.05,
                              epsilonDecaySteps: Int = 200_000,       // slower decay for better exploration
                              targetUpdateEvery: Int = 3_000,         // hard-copy period (if no softTau)
                              softTau: Option[Double] = Some(0.005),  // soft target update per step if defined
                              replayCapacity: Int = 500_000,
                              batchSize: Int = 64,
                              hidden: Int = 128,
                              // Stabilizers
                              clipQ: Double = 50.0,                   // clamp Q and targets to [-clipQ, clipQ]
                              rewardClip: Double = 5.0,               // clip per-step reward
                              gradClip: Double = 1.0,                 // gradient clipping (elementwise)
                              // Logging
                              logInterval: Int = 200,                 // steps
                              emaAlpha: Double = 0.1,                 // EMA smoothing for loss
                              // Compute/memory
                              checkpoint: Boolean = true              // gradient checkpointing for MLP
                            )

final case class TrainConfig(
                              epochs: Int = 50,
                              stepsPerEpoch: Int = 500,
                              evalEvery: Int = 5
                            )

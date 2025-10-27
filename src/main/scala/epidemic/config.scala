package epidemic

final case class HyperParams(
                              gamma: Double = 0.99,
                              lr: Double = 1e-3,
                              epsilonStart: Double = 1.0,
                              epsilonEnd: Double = 0.1,          // 0.05 for longer runs
                              epsilonDecaySteps: Int = 5000,     // slow down/raise for longer runs
                              targetUpdateEvery: Int = 1500,
                              softTau: Option[Double] = None,
                              replayCapacity: Int = 50000,
                              batchSize: Int = 64,
                              hidden: Int = 128,
                              clipQ: Double = 10.0,
                              rewardClip: Double = 5.0,
                              gradClip: Double = 1.0,
                              logInterval: Int = 500,
                              emaAlpha: Double = 0.0,
                              checkpoint: Boolean = true,
                              learnEvery: Int = 8                 // update cadence
                            )

final case class TrainConfig(
                              epochs: Int = 20,
                              stepsPerEpoch: Int = 800,
                              evalEvery: Int = 2
                            )

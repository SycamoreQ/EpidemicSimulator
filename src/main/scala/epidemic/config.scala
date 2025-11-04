package epidemic

final case class HyperParams(
                              gamma: Double = 0.99,
                              lr: Double = 1e-3,
                              epsilonStart: Double = 1.0,
                              epsilonEnd: Double = 0.1,
                              epsilonDecaySteps: Int = 100000,
                              targetUpdateEvery: Int = 1500,
                              softTau: Option[Double] = None,
                              replayCapacity: Int = 50000,
                              batchSize: Int = 128,
                              hidden: Int = 128,
                              clipQ: Double = 10.0,
                              rewardClip: Double = 5.0,
                              gradClip: Double = 1.0,
                              logInterval: Int = 500,
                              emaAlpha: Double = 0.0,
                              checkpoint: Boolean = true,
                              learnEvery: Int = 8
                            )

final case class TrainConfig(
                              epochs: Int = 20,
                              stepsPerEpoch: Int = 600,
                              evalEvery: Int = 10
                            )

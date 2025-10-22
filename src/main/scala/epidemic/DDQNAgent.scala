// src/main/scala/epidemic/DDQNAgent.scala
package epidemic

import epidemic.HyperParams 
import epidemic.MLP


final class DDQNAgent(hp: HyperParams, stateSize: Int, actionSize: Int) {
  private val online = new MLP(stateSize, hp.hidden, actionSize, hp.lr)
  private val target = new MLP(stateSize, hp.hidden, actionSize, hp.lr)
  private val replay = new ReplayBuffer(hp.replayCapacity)

  private var steps: Long = 0L
  private var emaLoss: Double = 0.0

  private def epsilon: Double = {
    val frac = math.min(1.0, steps.toDouble / math.max(1, hp.epsilonDecaySteps))
    hp.epsilonStart + (hp.epsilonEnd - hp.epsilonStart) * frac
  }

  private val rnd = new scala.util.Random(9)

  def act(s: State): Int = {
    steps += 1
    if (rnd.nextDouble() < epsilon) rnd.nextInt(actionSize)
    else online.predict(s.features, clamp = hp.clipQ).zipWithIndex.maxBy(_._1)._2
  }

  def observe(tr: Transition): Unit = replay.push(tr)

  private def clipQ(x: Double): Double = math.max(-hp.clipQ, math.min(hp.clipQ, x))

  private def ddqnTarget(exp: Transition): Array[Double] = {
    val q = online.predict(exp.s.features, clamp = hp.clipQ).clone()
    if (exp.done) q(exp.a) = clipQ(exp.r)
    else {
      val qn = online.predict(exp.s2.features, clamp = hp.clipQ)
      val best = qn.zipWithIndex.maxBy(_._1)._2
      val qtn = target.predict(exp.s2.features, clamp = hp.clipQ)
      q(exp.a) = clipQ(exp.r + hp.gamma * qtn(best))
    }
    q
  }

  def learn(batch: Int, gradAccumSteps: Int = 4): Double = {
    val need = math.max(batch * gradAccumSteps, batch)
    if (replay.size < need) return 0.0

    // Build one accumulated micro-batch
    val micros = replay.sample(need)
    val xs = micros.map(_.s.features)
    val tgts = micros.map(ddqnTarget)

    // Single update with accumulated grads
    val loss = online.trainBatch(xs, tgts, hp.gradClip, clamp = hp.clipQ)

    // Soft or hard target sync
    hp.softTau match {
      case Some(tau) => target.copyFrom(online) // or implement a softUpdate if added back
      case None => if (steps % hp.targetUpdateEvery == 0) target.copyFrom(online)
    }

    // EMA loss
    emaLoss = if (emaLoss == 0.0) loss else hp.emaAlpha * loss + (1 - hp.emaAlpha) * emaLoss
    loss
  }

  def epsilonNow: Double = epsilon
  def qValues(s: State): Array[Double] = online.predict(s.features, clamp = hp.clipQ)
  def emaLossNow: Double = emaLoss
}

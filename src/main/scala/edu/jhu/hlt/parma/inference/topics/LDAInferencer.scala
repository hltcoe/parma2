// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.inference.topics

import org.apache.commons.math3.special.Gamma

class LDAInferencer(_converged: Double, max_iter: Int) extends Serializable {

  val VAR_CONVERGED = _converged
  var VAR_MAX_ITER  = max_iter

  def infer(doc: Document, model: LDA, var_gamma: Array[Double], phi: Array[Array[Double]]) : Double = {
    
    val old_phi     = Array.ofDim[Double](model.num_topics)
    val digamma_gam = Array.ofDim[Double](model.num_topics)

    // compute posterior Dirichlet
    var k = 0
    while(k < model.num_topics) {
      var_gamma(k) = model.alpha + (doc.total.toDouble / model.num_topics)
      digamma_gam(k) = Gamma.digamma(var_gamma(k))
      var n = 0
      while(n < doc.length) {
        phi(n)(k) = 1.0 / model.num_topics
        n += 1
      }
      k += 1
    }

    var var_iter = 0;
    var converged = 1.0;
    var likelihood = 0.0
    var likelihood_old = 0.0;

    while((converged > VAR_CONVERGED) && ((var_iter  < VAR_MAX_ITER) || (VAR_MAX_ITER == -1))) {
      var_iter += 1
      var n = 0
      while(n < doc.length) {
        var phi_sum = 0.0
        var k = 0
        while(k < model.num_topics) {
          old_phi(k) = phi(n)(k)
          phi(n)(k) = digamma_gam(k) + model.log_prob_w(k)(doc.words(n))

          if(k > 0) {
            phi_sum = logSum(phi_sum, phi(n)(k))
          } else {
            phi_sum = phi(n)(k) // phi is in log space
          }

  //        println("phi_sum = " + phi_sum)

          k += 1
        }

//        println("doc counts = " + doc.counts(n))
        k = 0
        while(k < model.num_topics) {
          phi(n)(k) = Math.exp(phi(n)(k) - phi_sum)
          var_gamma(k) += doc.counts(n) * (phi(n)(k) - old_phi(k))
//          println("phi = " + phi(n)(k) + ", old_phi = " + old_phi(k) + ", var gamma = " + var_gamma(k)) // XXX
          digamma_gam(k) = Gamma.digamma(var_gamma(k))
          k += 1
        }
        // XXX Debug
  //       k = 0
  //       while(k < model.num_topics) {
  // //        println("digamma_gam = " + digamma_gam(k))
  //         k += 1
  //       }
  //       exit(1)
        // XXX

        n += 1
      }

      likelihood     = computeLikelihood(doc, model, phi, var_gamma)
//      println("[LDA INF] likelihood = " + likelihood)
//      exit(1)
      converged      = (likelihood_old - likelihood) / likelihood_old
      likelihood_old = likelihood
    }

    likelihood
  }

  def logSum(log_a: Double, log_b: Double) : Double = {
    if(log_a < log_b) {
      log_b + Math.log(1.0 + Math.exp(log_a-log_b))
    } else {
      log_a + Math.log(1.0 + Math.exp(log_b-log_a))
    }
  }

  def computeLikelihood(doc: Document, model: LDA, phi: Array[Array[Double]], var_gamma: Array[Double]) : Double = {
    val dig = Array.ofDim[Double](model.num_topics)
    var k = 0
    var var_gamma_sum = 0.0
    while(k < model.num_topics) {
      dig(k) = Gamma.digamma(var_gamma(k))
      var_gamma_sum += var_gamma(k)
      k += 1
    }
    val digsum = Gamma.digamma(var_gamma_sum)
    var likelihood = Gamma.logGamma(model.alpha * model.num_topics)
                     - model.num_topics * Gamma.logGamma(model.alpha)
                     - Gamma.logGamma(var_gamma_sum)

    k = 0
    while(k < model.num_topics) {
      likelihood += (model.alpha-1.0)*(dig(k)-digsum) 
                 +  Gamma.logGamma(var_gamma(k))
                 -  (var_gamma(k)-1.0)*(dig(k)-digsum)
      var n = 0
      while(n < doc.length) {
        if(phi(n)(k) > 0) {
          likelihood += doc.counts(n) * (phi(n)(k)*((dig(k) - digsum) - Math.log(phi(n)(k))
                                         + model.log_prob_w(k)(doc.words(n))))
        }
        n += 1
      }
      k += 1
    }
    likelihood
  }
}

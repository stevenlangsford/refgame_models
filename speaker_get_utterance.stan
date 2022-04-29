functions {
  real single_utterance_prob(real mystatus, //int, here real only bc stored in matrix
			     real my_neighbourcount,//also in matrix, + divides probs
			     real p_under,
			     real p_over){
        //GUTS OF THE JE MODEL:
   /*  To calculate this likelihood term, we first assume that the speaker */
/* has a fixed and known probability of accidentally producing an un- */
/* derinformative expression (e.g., “the triangle” when two triangles */
/* are in view), estimated in a separate task (P = 0.055 and 0.056 for */
/* experiments 1 and 2, respectively, and P = 0.047, 0.163, and 0.217 */
/* for color, size, and category in experiment 3; see the Supplementary */
/* Materials for experiment details). In the remaining cases, the speaker */
/* selects the shortest sufficiently informative utterance with probability */
/* 1 – r and introduces a redundant adjective with probability r. Criti- */
/* cally, however, we treat this probability as variable across speakers */

    //status code conventions: 1=bad, 2=underinform, 3=overinform, 4=justright

    int verbose;
    verbose = 0;
    if(verbose)print("single prob ",mystatus, ":",
		     my_neighbourcount, ":",
		     p_under,":", p_over
		     );
    if(verbose)print("single_utterance_prob says");
    if(verbose)print("my_status", mystatus);
    if(verbose)print("my_hood", my_neighbourcount);
    if(verbose)print("under", p_under);
    if(verbose)print("over", p_over);
    
    if(mystatus == 1){
      if(verbose) print("ret 0");
      return(0);//maybe should be eps? zero is scary
    }
    if(mystatus==2){
      if(verbose)print("ret 2");
      if(verbose) print(p_under/my_neighbourcount);
      return(p_under/my_neighbourcount);
    }
    if(mystatus==3){
      if(verbose) print("ret 3");
      if(verbose) print( ((1-p_under) * p_over)/my_neighbourcount);
      return( ((1-p_under) * p_over)/my_neighbourcount);
    }
    if(mystatus==4){
      if(verbose) print("ret 4");
      if(verbose) print(((1-p_under) * (1-p_over))/my_neighbourcount);
      return(((1-p_under) * (1-p_over))/my_neighbourcount);
    }
    if(verbose)print("STATUS WASHOUT");
    return(-1);//compiler-candy. Better to throw an error, what's the stan for that?
  }
  
  row_vector all_utterance_probs(row_vector status_list,
  				 row_vector status_count,
  				 real p_under,
  				 real p_over,
  				 int n_utterances
  			     ){
    real normsum;
    row_vector[n_utterances] ret;
    for (i in 1:n_utterances){
      ret[i] = single_utterance_prob(status_list[i],
  				     status_count[i],
  				     p_under,
  				     p_over);
    }
    normsum = sum(ret);
    for(i in 1:n_utterances){
      //There are often no consistent-underinformative utterances: one word is enough
      //Then p_under is not as per the param, it's zero, no valid continuations
      //Normalizing ret catches these cases where the tree is not full.
      //But surely there's a better way to implement than this?
      ret[i] = ret[i] / normsum;
    }
    return(ret);
  }//all utterance probs

  vector stacked_utterance_probs(matrix status_lists,
				 matrix status_counts,
				 vector ref_probs,
				 real p_under,
				 real p_over,
				 int n_utterances,
				 int n_options
				 ){
    row_vector[n_utterances] ret;
    row_vector[n_utterances] singleworld;
    int verbose;
    verbose = 0;
    
    for(i in 1:n_utterances){
      ret[i] = 0;//init
      //   diag_singleworld[i]=0;
    }
    
  for(i in 1:n_options) {
    singleworld = all_utterance_probs(status_lists[i,],
				      status_counts[i,],
				      p_under,
				      p_over,
				      n_utterances);
   if(verbose)print("singleworld ",i," sum is ",sum(singleworld)); //SHOULD BE 1, OK FOR TARGET BUT NOT FOR OPTIONS 2 AND 3 HERE BE DRAGONS
    if(verbose)print("status list is");
    if(verbose)print(status_lists[i,]);
    //print(status_counts[i,]);
    if(verbose)print("p_under is ",p_under);
    if(verbose)print("p_over is ",p_over);
    if(verbose)print("ref_prob is ",ref_probs[i]);
    if(verbose)print("add to ret ", singleworld * ref_probs[i]);

    ret = ret + singleworld * ref_probs[i];

    if(verbose)print("current ret ", ret);
    //    print("refprob at ",i," is ", ref_probs[i]);
  }//for each option
  if(verbose)print("stacked_utterance_probs");
  if(verbose)print("final ret: ", ret, " with sum ",sum(ret));
  if(verbose)print("***");
  
  return(to_vector(ret));
  }
}//functions block

data {
  int n_trials;
  int n_options;
  int n_utterances;
    
  matrix[n_options, n_utterances] status_lists[n_trials];
  matrix[n_options, n_utterances] status_counts[n_trials];

  //  int obs[n_trials];

  //pass in 'param' values: to recover
  real p_under;
  real p_over;
  simplex[n_options] ref_probs[n_trials];//Ref is known, so each ref_prob is 0 or 1, passing in this format to avoid changing anything.
}

parameters {
  //  simplex[n_options] ref_probs[n_trials];
  //  real<lower=0, upper=1> p_under;
  //  real<lower=0, upper=1> p_over;
  real placeholder;
}

model {
  placeholder~normal(0,1);//just algorithm=fixed_param fails bc treedepth not found in output... hackfix, just can't deal rn, whatevs
  
  //priors: no need, sim truth passed as args
  /* for(i in 1:n_trials){ */
  /* ref_probs[i] ~ dirichlet(rep_vector(1, n_options)); */
  /* } */
  
  /* p_under ~ beta(1,10); */
  /* p_over ~ beta(1,10); */

  /* for(i in 1:n_trials){ */
  /* obs[i] ~ categorical(stacked_utterance_probs(status_lists[i], */
  /* 					    status_counts[i], */
  /* 					    ref_probs[i], */
  /* 					    p_under, */
  /* 					    p_over, */
  /* 					    n_utterances, */
  /* 					    n_options)); */
  /* } */
}

generated quantities{
  int speaker_utterance[n_trials];
  //int posterior_ref[n_trials];
  //  real prior_beta; //only really need to run once, report & delete

  for(i in 1:n_trials) {
  speaker_utterance[i] = categorical_rng(stacked_utterance_probs(
							  status_lists[i],
							  status_counts[i],
							  ref_probs[i],
							  p_under,
							  p_over,
							  n_utterances,
							  n_options)
				  );

  //posterior_ref[i] = categorical_rng(ref_probs[i]);
  }//end for each trial
  
  //  prior_beta = beta_rng(1,10);
}

importScripts('https://cdn.jsdelivr.net/npm/lodash@4.17.21/lodash.min.js');


function RWrapper(pass_helpers) {
   
    let helpers={};
    let webR = null;
    let renv = null;









async function add_helper(name, body) {
  console.log("add_helper: " + name);
  
  helpers[name] = await webR.evalR(body);


}


async function add_helpers(pass_helpers) {
  console.log("add_helpers");
  var names = Object.keys(pass_helpers);
  for (var i = 0; i < names.length; i++) {
    await add_helper(names[i], pass_helpers[names[i]]);
  }

}


async function submit(data) {
  console.log("submit1");
  console.log(data);
  const result = await helpers[data.helper].exec(renv, JSON.stringify(data));
  const js_obj = await result.toJs();
  postMessage(js_obj);
  console.log("submit2");

}


function about() {
  return "v0.0.0"
}




function load_tools_worker(pass_helpers) {
  import('/js/webr.mjs').then(
      async ({ WebR }) => {
          webR = new WebR();
          await webR.init();
          await webR.evalR(`webr::install("jsonlite")`);
          renv = await webR.evalR(`new.env()`)

          await add_helpers(pass_helpers);

          console.log('Worker: webR loaded');
          var temp = { type: "character", names: null, values: ["webR loaded"] }
          postMessage( temp);
          
      }
  );
}

var debounced_load_tools_worker = _.debounce(load_tools_worker, 1000, { 'maxWait': 1000 })




function load_tools(pass_helpers) {
 
  debounced_load_tools_worker(pass_helpers);

}
load_tools(pass_helpers);



return { about: about,submit: submit };

}




var helpers = {};
    
helpers["beta_binomial_single_posterior"] = `
function(renv, data) { 
    x <- jsonlite::parse_json(data)

   
    df=x$df
    m=as.data.frame(matrix(as.numeric(unlist(df)),ncol=2, byrow = T))

            m = m[is.na(m[,1])==F,,drop=F]
            k=m[,1]
            n=m[,2]
            m[,3] <- k/n
            m[,4] <- qbeta(.5,k+1,(n-k)+1)
            m[,5] <- qbeta(.025,k+1,(n-k)+1)
            m[,6] <- qbeta(.975,k+1,(n-k)+1)

            x$output_obj <- m
        
        return(
            jsonlite::toJSON(x,dataframe = c("values")) 
            )
    
    }
        `;


//         helpers["beta_binomial_single_posterior"] = `
// function(renv,data) { 
//     if("z" %in% ls(envir=renv)){
//         renv$z=renv$z+1
//     }else{
//         renv$z=0
//     }

//     x <- jsonlite::parse_json(data)

//         return(paste0("it worked ",renv$z," | ", jsonlite::toJSON(x))) 
    
//     }
//         `;





  
      
helpers["nps_testing_function"] = `
function(renv, data) { 


    rdirichlet <- function (n, alpha) 
    {
        l <- length(alpha)
        x <- matrix(rgamma(l * n, alpha), ncol = l, byrow = TRUE)
        sm <- x %*% rep(1, l)
        x/as.vector(sm)
    }

    assert <- function(logic_val, stop_message) {
        if (length(logic_val) != 1) {
          stop(stop_message, call. = F)
        }
        if (is.logical(logic_val) == F) {
          stop(stop_message, call. = F)
        }
        if (is.na(logic_val) == T) {
          stop(stop_message, call. = F)
        }
      
        if (logic_val == F) {
          stop(stop_message, call. = F)
        }
      }
      
    util_nps_confidence <- function(promoters, passives, detractors,prior = 1, n_samples = 10000, probs = c(0.025, 0.975)) {
        assert(length(promoters) == length(passives), "Lengths must be the same")
        assert(length(promoters) == length(detractors), "Lengths must be the same")
       
        my_out <- vector("list", length(promoters))
        for (i in seq_len(length(promoters))) {

          my_out[[i]] <- util_nps_confidence_inner(promoters = promoters[i], passives = passives[i], detractors = detractors[i], prior, n_samples, probs)
          my_out[[i]]$promoters <- promoters[i]
          my_out[[i]]$passives <- passives[i]
          my_out[[i]]$detractors <- detractors[i]

        }
        
        out_df <- do.call(rbind, my_out)
        out_df <- out_df[c(c("promoters", "passives", "detractors"), setdiff(colnames(out_df), c("promoters", "passives", "detractors")))]
        return(out_df)
      }
      
      
      util_nps_confidence_inner <- function(promoters, passives, detractors, prior, n_samples, probs) {
        survey_responses <- c(promoters, passives, detractors)
        alpha <- survey_responses + prior
        samples <- u__sample_dirichlet_multinomial(n_samples, alpha, sum(survey_responses))
      
      
      raw_nps <- u__calculate_nps(setNames(survey_responses, c("promoters", "passives", "detractors"))) 
      
      
        nps_samples <- apply(samples, 1, function(row) u__calculate_nps(setNames(row, c("promoters", "passives", "detractors"))))
        nps_mean <- mean(nps_samples)
        nps_ci <- quantile(nps_samples, probs)
      
        out <- matrix(c(raw_nps,nps_mean, nps_ci), nrow = 1)
        out_df <- as.data.frame(out)
        colnames(out_df) <- c("nps_raw","nps_mean", paste0("nps_p_", probs))
        return(out_df)
      }
      u__calculate_nps <- function(counts) {
        total <- sum(counts)
        promoters <- counts["promoters"] / total
        detractors <- counts["detractors"] / total
        nps <- (promoters - detractors) * 100
        return(nps)
      }
      
      u__sample_dirichlet_multinomial <- function(n, alpha, size) {
        # Sample from Dirichlet distribution
        p <- rdirichlet(n, alpha)
      
        # Initialize the result matrix
        samples <- matrix(0, nrow = n, ncol = length(alpha))
      
        # Sample from Multinomial distribution for each row of p
        for (i in 1:n) {
          samples[i, ] <- stats::rmultinom(1, size, p[i, ])
        }
      
        return(samples)
      }
      
      





    x <- jsonlite::parse_json(data)

   
    df=x$df
    random_seed <- as.integer(x$random_seed)
    sample_size <- as.integer(x$sample_size)

    m=as.data.frame(matrix(as.numeric(unlist(df)),ncol=3, byrow = T))
    m = m[is.na(m[,1])==F,,drop=F]




    promoters=m[,1]
    passives=m[,2]
    detractors=m[,3]


    new_seed <- sample.int(.Machine$integer.max, 1)
    set.seed(random_seed)
    m <- util_nps_confidence(promoters, passives, detractors,prior = 1, n_samples = sample_size, probs = c(0.025, 0.975)) 

    set.seed(new_seed)

            x$output_obj <- m
        
        return(
            jsonlite::toJSON(x,dataframe = c("values")) 
            )
    
    }
        `;


console.log("start main.js");
var r_wrapper = RWrapper(helpers);



onmessage = function (e) {
    console.log("onmessage1");
    r_wrapper.submit(e.data);
    console.log("onmessage2");
    // console.log('Worker: Message received from main script');
    // const result = e.data[0] * e.data[1];
    // if (isNaN(result)) {
    //     postMessage('Please write two numbers');
    // } else {
    //     const workerResult = 'Result: ' + result;
    //     console.log('Worker: Posting message back to main script');
    //     postMessage(workerResult);
    // }
}
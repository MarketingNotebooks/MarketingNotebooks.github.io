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
  import('https://webr.r-wasm.org/latest/webr.mjs').then(
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
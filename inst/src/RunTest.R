con <- file("/tmp/computer","r")
COMPUTER_NAME <- readLines(con,n=1)
close(con)
Sys.setenv(COMPUTER=COMPUTER_NAME)

for(baseFolder in c("/data_clean","/results","/data_app")){
  files <- list.files(file.path(baseFolder,"brain"))
  if(length(files)>0){
    for(f in files) unlink(file.path(baseFolder,"brain",f))
  }
}

unlink(file.path("/junit","brain.xml"))
Sys.sleep(1)

a <- testthat:::JunitReporter$new()
a$start_reporter()
a$out <- file(file.path("/junit","brain.xml"), "w+")
a$start_context("brain")

output <- processx::run("Rscript","/r/brain/src/RunProcess.R", error_on_status=F, echo=T)
cat("\n\nstdout\n\n")
cat(output$stdout)
cat("\n\nstderr\n\n")
cat(output$stderr)

if(output$status==0){
  a$add_result("brain","RunAll",testthat::expectation("success","Pass"))
} else {
  a$add_result("brain","RunAll",testthat::expectation("error","Fail"))
}

a$end_context("brain")
a$end_reporter()
close(a$out)




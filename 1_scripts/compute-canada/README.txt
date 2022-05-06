This is the code for running BRT models and making predictions in a parallel
computing environment. In this case, on Compute Canada. 

Files numbers corresposnd to the regular R files, but with cc in front. 

In some cases, tasks done in regular R files are broken down into sub-tasks. 
It should be obvious where that has been done. 

If there is no BASH file associated with an R file, it means that file can 
be run in the interactive environment. This usually means it is creating R
files and BASH files for submitting many jobs across all species. 

For submitting many jobs at once from the same folder (such as for the predictions), 
there is a .txt file called 'submit_array_jobs.txt' that can be pasted 
into the shell, and will submit all jobs in that folder. 




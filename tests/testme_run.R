# This runs tests `testme`
#testmeEnvInit('RtestsExpectations', logger = print);
library('testme');
if (Sys.getenv('NOT_ON_CRAN') == '1')) print(testmeFileSingle('testme/testme.R', 'testme/RtestsExpectations', useGit = FALSE, logger = print));

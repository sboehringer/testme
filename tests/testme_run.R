# This runs tests `testme`
#testmeEnvInit('RtestsExpectations', logger = print);
library('testme');
print(testmeFileSingle('testme/testme.R', 'testme/RtestsExpectations', useGit = FALSE, logger = print));

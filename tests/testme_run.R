# This runs tests `testme`
#testmeEnvInit('RtestsExpectations', logger = print);
library('testme');
print(testmeFileSingle('testme.R', 'RtestsExpectations', useGit = FALSE, logger = print));

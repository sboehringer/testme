# This runs tests `RtestsTestme`
#testmeEnvInit('RtestsExpectations', logger = print);
library('testme');
print(testmeFileSingle('RtestsTestme.R', 'RtestsExpectations', useGit = FALSE, logger = print));

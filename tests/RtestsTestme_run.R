# This runs tests `RtestsTestme`
#testmeEnvInit('RtestsExpectations', logger = print);
print(testmeFileSingle('RtestsTestme.R', 'RtestsExpectations', useGit = FALSE, logger = print));

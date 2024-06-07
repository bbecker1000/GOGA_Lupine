#Github test

#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

## set your user name and email:
usethis::use_git_config(user.name = "ClaireWhicker", user.email = "clairewhicker@berkeley.edu")

## create a personal access token for authentication:
usethis::create_github_token() 
## in case usethis version < 2.0.0: usethis::browse_github_token() (or even better: update usethis!)

## Note for Linux users:
## credentials::set_github_pat() (in line 34) might store your PAT in a memory cache that
## expires after 15 minutes or when the computer is rebooted. You thus may wish to do 
## extend the cache timeout to match the PAT validity period:
usethis::use_git_config(helper="cache --timeout=2600000") #> cache timeout ~30 days

## set personal access token:
credentials::set_github_pat("YourPAT")

## or store it manually in '.Renviron':
usethis::edit_r_environ()
## store your personal access token in the file that opens in your editor with:
## GITHUB_PAT=xxxyyyzzz
## and make sure '.Renviron' ends with a newline
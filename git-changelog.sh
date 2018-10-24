#/bin/bash
git log --no-merges --pretty=format:"%n%w(72,2,4)[%an]  %cD%n%n%w(72,2,4)* %s"

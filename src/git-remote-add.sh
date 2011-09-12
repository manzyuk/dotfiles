#!/usr/bin/env bash

# Usage: git-remote-add [<repo-name>]
#
# Create a bare repository on golconda and add it as a remote to the
# current local repository.  The name of the repository is either
# inferred from the directory name or is supplied explicitly.

if [[ -z $1 ]]; then
    repo_name=$(basename `pwd`)
else
    repo_name=$1
fi

repo_path="git/${repo_name}.git"
command="mkdir -p ${repo_path}; cd ${repo_path}; git init --bare"

ssh golconda $command

(git remote | grep golconda > /dev/null) && git remote rm golconda
git remote add golconda manzyuk@golconda.cs.nuim.ie:${repo_path}
<a href="https://builds.sr.ht/~akagi/srht.el/commits/master/.build.yml"><img alt="Build" src="https://builds.sr.ht/~akagi/srht.el/commits/master/.build.yml.svg"/></a>


### Version 0.1


# About

GNU Emacs [sourcehut](https://sr.ht) API client.

`srht` provides bindings to the Sourcehut REST API as well as commands for
interacting with it. It currently supports two services: `git.sr.ht` — git
hosting and `paste.sr.ht` — ad-hoc text file hosting.


# Installation


## With Guix

    git clone https://git.sr.ht/~akagi/srht.el srht
    cd srth
    guix package -f guix.scm


## Manual

`srht` depends on the HTTP library `plz` which is available in ELPA. After
installing it, place files from /lisp folder in `load-path`.


# Setup

To use this client, you need to [generate](https://meta.sr.ht/oauth/personal-token) a personal access token. This token
will have unrestricted access to all sr.ht APIs and can be used like a normal
access token to authenticate API requests.

After creating the token:

    (setq srht-token TOKEN)

It is also possible to store the token using `auth-source.el`, the host must be
set to sr.ht.

    machine sr.ht password TOKEN

You also need to set srht-username:

    (setq srht-username USERNAME)

If you are using a self-hosted instanse:

    (setq srht-domain DOMAIN)


# Commands

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Function</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left"><code>srht-git-repo-update</code></td>
<td class="org-left">Update information for git repository</td>
</tr>


<tr>
<td class="org-left"><code>srht-git-repo-delete</code></td>
<td class="org-left">Delete existing git repository</td>
</tr>


<tr>
<td class="org-left"><code>srht-git-repo-create</code></td>
<td class="org-left">Create git repository</td>
</tr>


<tr>
<td class="org-left"><code>srht-paste-link</code></td>
<td class="org-left">Kill the link of the selected paste</td>
</tr>


<tr>
<td class="org-left"><code>srht-paste-delete</code></td>
<td class="org-left">Detete paste with SHA</td>
</tr>


<tr>
<td class="org-left"><code>srht-paste-region</code></td>
<td class="org-left">Paste region or buffer to sourcehut</td>
</tr>
</tbody>
</table>


# License

GPLv3

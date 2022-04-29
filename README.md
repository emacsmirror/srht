

### Version 0.1.0


# About

GNU Emacs [sourcehut](https://sr.ht) API client.


# Setup

To use this client, you need to [generate](https://meta.sr.ht/oauth/personal-token) a personal access token. This token
will have unrestricted access to all sr.ht APIs and can be used like a normal
access token to authenticate API requests.

After creating the token:

    (setq srht-token TOKEN)

It is also possible to store the token using auth-source.el, the host must be
set to paste.sr.ht.

    machine paste.sr.ht password TOKEN

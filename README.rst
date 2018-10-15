=================
 trivial-imap
=================

.. insert-your badges like that:

.. image:: https://travis-ci.org/40ants/trivial-imap.svg?branch=master
    :target: https://travis-ci.org/40ants/trivial-imap

.. Everything starting from this commit will be inserted into the
   index page of the HTML documentation.
.. include-from

This is a thin wrapper over post-office library (which is a fork of
Franz's cl-imap). Trivial-imap tries to make easy some common cases of
working with IMAP servers, like reading emails from the server.

Reasoning
=========

Libraries like ``post-office`` or ``mel-base`` are not easy to
use. Especially this is sad when you only need to do such simple task as
read emails with their content.

Here is how such code will look like with ``trivial-imap``:

.. code-block:: common-lisp-repl

   CL-USER> (trivial-imap:fetch-messages "imap.gmail.com"
                                         "svetlyak.40wt"
                                         *password*
                                         :folder "Autoprocessing/OrgModeInbox"
                                         :limit 3)
   (#<TRIVIAL-IMAP/CORE:EMAIL uid=22 subject="Tweet from The Little Lisper (@thelittlelisper)">
    #<TRIVIAL-IMAP/CORE:EMAIL uid=25 subject="The Animated Guide to Paredit">
    #<TRIVIAL-IMAP/CORE:EMAIL uid=26 subject="Tweet from benoît chesneau (@benoitc)">)
   1500 (11 bits, #x5DC)
   CL-USER> (trivial-imap:get-subject (second #v11))
   "The Animated Guide to Paredit"
   CL-USER> (trivial-imap:get-text (second #v11))
   "danmidwood.com/content/2014/11/21/animated-paredit.html 
   
   Download the official Twitter app here
   
   
   Sent from my iPad
   "
   CL-USER> (trivial-imap:get-html (second #v11))
   "<html><head><meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\"></head><body dir=\"auto\"><div><a href=\"http://danmidwood.com/content/2014/11/21/animated-paredit.html\">danmidwood.com/content/2014/11/21/animated-paredit.html</a>
   <br><br>Download the official Twitter app <a href=\"https://twitter.com/download?ref_src=MailTweet-iOS\">here</a>
   </div><div><br><br>Sent from my iPad</div></body></html>
   "

Also, you can use ``:since-uid`` argument, to iterate through all
messages in the Inbox.

.. code-block:: common-lisp-repl

   CL-USER> (trivial-imap:fetch-messages "imap.gmail.com"
                                         "svetlyak.40wt"
                                         *password*
                                         :folder "Autoprocessing/OrgModeInbox"
                                         :limit 3
                                         :since-uid (trivial-imap:get-uid (second #v11)))
   (#<TRIVIAL-IMAP/CORE:EMAIL uid=26 subject="Tweet from benoît chesneau (@benoitc)">
    #<TRIVIAL-IMAP/CORE:EMAIL uid=27 subject="Заметка про Lisp">
    #<TRIVIAL-IMAP/CORE:EMAIL uid=28 subject="Другие идеи">)
   1498 (11 bits, #x5DA)

Ideas
=====

* Return a closure allowing to fetch next page as a third value from
  fetch-messages.
* Support operation on emails, such as deletion or marking them as read.
* ...

.. Everything after this comment will be omitted from HTML docs.
.. include-to

Building Documentation
======================

Provide instruction how to build or use your library.

How to build documentation
--------------------------

To build documentation, you need a Sphinx. It is
documentaion building tool written in Python.

To install it, you need a virtualenv. Read
this instructions
`how to install it
<https://virtualenv.pypa.io/en/stable/installation/#installation>`_.

Also, you'll need a `cl-launch <http://www.cliki.net/CL-Launch>`_.
It is used by documentation tool to run a script which extracts
documentation strings from lisp systems.

Run these commands to build documentation::

  virtualenv --python python2.7 env
  source env/bin/activate
  pip install -r docs/requirements.txt
  invoke build_docs

These commands will create a virtual environment and
install some python libraries there. Command ``invoke build_docs``
will build documentation and upload it to the GitHub, by replacing
the content of the ``gh-pages`` branch.


Authors
=======

* Alexander Artemenko (svetlyak.40wt@gmail.com)

Copyright
=========

Copyright (c) 2018 Alexander Artemenko (svetlyak.40wt@gmail.com)

License
=======

Licensed under the BSD License.

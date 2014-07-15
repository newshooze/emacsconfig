  ------------------
  stef-elisp   v2.28
  April 4, 2014
  ------------------

  ... this is an archive of all my public Emacs Lisp code.
   you may not need all of it, but because of multiple dependencies it
   is much easier for me to deliver everything as a single bundle.


 * most of this code is intended for work in sound synthesis and
   musical composition with Keykit, Timidity and Csound. 

   for full documentation, including installation directives,
   please refer to this pages:

          http://www.zogotounga.net/comp/eindex.html
          http://www.zogotounga.net/surmulot/surmulot.html


 * some general purpose libraries are also provided: 
	query-sheet.el
	embedded-elisp-library.el
	comm-tests.el

   those are documented by comments at the top of the code files
   (you will also find examples of query-sheet usage in keykit-mode.el
   and csound-csd.el)


 * one undocumented work in progress is also present:
   the Emacs side of Surmulot (an integrated musical composition
   workshop including Squeak and Keykit)



   please report bugs; feedback is always welcome


                       Stef (St�phane Rollandin) <hepta@zogotounga.net>



  +---------------------------------------------------------------------+
  |                       files dependencies:                           |
  +---------------------------------------------------------------------+
  |                                                                     |
  |        keykit-mode requires: keykit-mode.el                         |
  |                              kk-utils.el                            |
  |                              query-sheet.el                         |
  |                                                                     |
  |        csound-x requires: the csound-x/ folder contents             |
  |                           query-sheet.el                            |
  |                           embedded-elisp-library.el        |
  |                           + (opt.) keykit-mode                      |
  |                                                                     |
  |        query-sheet.el                                               | 
  |        embedded-elisp-library.el                                    |
  |        comm-tests.el                                                |
  |        ... are all self-consistent                                  |
  |                                                                     |
  |        the files in folder surmulot/ belong to the Surmulot system  |
  |        http://www.zogotounga.net/surmulot/surmulot.html             |
  |                                                                     |
  +---------------------------------------------------------------------+


  --> to install just everything, unpack this archive in your site-lisp
      directory or anywhere else in your load-path, 
      then write the following in your .emacs and evaluate it: 

	  (require 'stef-elisp)


  ... most packages need correct custom settings in order to interface 
      properly with your system. see the HTML docs for details. 

      use the following commands to get to the top customization groups:

       M-x customize-group csound-x
       M-x customize-group keykit


  ... the csound-x package requires external components
      see the HTML doc (or Info files in /csound-x) for details



 


muttells
========

These little tools were written for users of the [Mutt email client](http://www.mutt.org). I wrote them because I have been wanting to learn some Haskell, so beware of newbie mistakes. But they might be useful to others.

Ideas and comments about this learning exercise are welcome. If anyone does [download and use the binaries](https://github.com/wcaleb/muttells/releases/latest) of `aliascheck` and `addalias` I have released, I would especially appreciate it if you could test them on your Mutt install and report any issues you encounter.

## addalias

This program takes one command line argument, a filepath to a Mutt aliases file.

It is meant to be used as a [display filter](http://www.mutt.org/doc/devel/manual.html#display-filter), with a line like the following in your muttrc:

	set display_filter="addalias /path/to/aliases/file"

Once so configured, every time you display or open a message in Mutt, `addalias` will grab the "From" line from the headers, construct an alias for the sender, check to see if it's formatted correctly, and then add it to your aliases file, so long as the exact alias does not already exist in that file.

## aliascheck

This program reads aliases or an alias file from `stdin` and checks each line to see if it is a [properly formatted alias](http://dev.mutt.org/trac/wiki/MuttGuide/Aliases). It then outputs the lines, prefixing a pound sign in front of invalid lines to "comment them out."

Example usage:

	$ cat "alias mcdaniel-caleb calebmcd@gmail.com" | aliascheck
	> alias mcdaniel-caleb calebmcd@gmail.com

	$ cat "alias mcdaniel caleb" | aliascheck
	> #alias mcdaniel caleb

	$ aliascheck < aliases.txt
	> alias mcdaniel-caleb calebmcd@gmail.com
	> #alias mcdaniel caleb
	> alias work mcdaniel-caleb, smith-joe, doe-jane
	> alias doe-jane Jane Doe jane.doe@work.com

In practice, the best way to use this may be by setting your muttrc file to source an alias file that has first been checked to make sure all the aliases are valid. This will prevent you from accidentally auto-expanding aliases that are actually invalid and then having to correct them in the address lines of your message before sending.

You can use Mutt's config file "backtick" feature to generate a temporary, validated alias file every time Mutt launches:

	source `aliascheck < ~/.mutt/aliases.txt > /tmp/valid-aliases.txt; echo /tmp/valid-aliases.txt`

Since the program is a filter, you can also call it from within an editor like Vim while editing your mutt file. Commented out lines will be sorted and listed at the top so that you can delete invalid aliases from time to time.

## muttdoc

This script is very rudimentary, but what I'm hoping is to write something capable of pretty printing emails, sort of like [muttprint](http://muttprint.sourceforge.net).

If you want to use it as is, compile `muttdoc.hs` using `ghc --make`, and then set your Mutt `print_command` to something like this:

	set print_command="muttdoc | pandoc --template=email.tex -V fullpage --latex-engine=xelatex --listings -o ~/Desktop/email.pdf

The LaTeX template used in that command is [in my pandoc-templates repo](https://github.com/wcaleb/pandoc-templates/blob/master/email.tex).



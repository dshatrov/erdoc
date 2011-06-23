#!/usr/bin/perl

#   erDoc - Documentation extractor for C/C++
#   Copyright (C) 2011 Dmitry Shatrov
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License along
#   with this program; if not, write to the Free Software Foundation, Inc.,
#   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


use strict;

binmode (STDIN,  ":utf8");
binmode (STDOUT, ":utf8");

# This is a universal procedure for extracting declarations
# of any kind (classes, functions, data members, etc.)
#
# We use this procedure when extracting forced declarations
# (the ones which are explicitly identified with an erdoc comment).
#
# Note that for auto-detected declarations, this procedure is
# excessive: such declarations are extracted twice, but that's
# not an issue. There is some unfortunate functionality duplication,
# though.
#
# TODO Declaration escapement (? what's that?)
sub extract_decl {
    my $line = shift @_;
    my $ignore_typedef = shift @_;

    my $is_typedef;
    my $is_typedef_block;
    my $got_typedef_name;
    my $typedef_line;

    my $decl_lines;
    my $typedef_name;

    my $read_ahead;
    while () {
	if (not $line) {
	    $line = <STDIN>;
	    last if not $line;
	    $read_ahead .= $line;
	}

	my $decl_line;
	my $eod;

	if (!$ignore_typedef and
	    !$is_typedef     and
	    $line =~ /(?:^|\s)typedef(?:\s|$)/)
	{
	  # We handle declarations like this specially:
	  #     typedef enum {
	  #         ...
	  #     } MyEnum;

	    $is_typedef = 1;

	    $line =~ /(?:^|\s)(typedef.*?)(?:;|({))/s;
	    if ($2) {
		$typedef_line = $1.$2." ";
		$is_typedef_block = 1;
	    } else {
		$typedef_line = $1;
	    }
	}

	# Note: For typedefs, this is just plain wrong,
	#       because nested classes are not handled.
	if ($is_typedef &&
	    ($is_typedef_block ? $line =~ /^\s*(?:})\s*(.*?)(;)/s :
				 $line =~ /^(.*?)(;)/s))
	{
	    my $typedef_ending = $1;
	    if (!($typedef_ending =~ /(?:^|\s)typedef(?:\s|$)/)) {
		$decl_line = $typedef_line;
	    }

	    if ($is_typedef_block) {
		$typedef_ending = "... } $typedef_ending";
	    }

	    $decl_line .= $typedef_ending;

	    $decl_line =~ /([^\s]+)\s*$/s;
	    $typedef_name = $1;

	    $eod = 1;

	    $decl_lines = $decl_line;
	} elsif (!$is_typedef &&
		 $line =~ /^(.*?)(;|{)/s)
	{
	    $decl_line = $1;
	    $eod = $2;
	} else {
	    $decl_line = $line;
	}

	if (!$is_typedef &&
	    $decl_line =~ /[^\s]+/s)
	{
	    $decl_lines .= $decl_line;
	}

	# TODO Isn't this a bug?
	#      We don't strip the beginning of $line, which belongs
	#      to the declaration, and schedule it for further processing.
	last if $eod;
	$line = '';
    }

    if ($typedef_name) {
	print ".name $typedef_name\n";
    }

    print ".decl\n$decl_lines\n";

    return $read_ahead;
}

# 1 - in the process of gathering a declaration.
my $parser_decl_mode = 0;

# If we've got only a part
my $parser_incomplete_line;
=for
# Type of declaration currently being extracted
# ("class" / "struct" / "union" / "enum")
my $parser_decl_type;
=cut
# Accumulator for the declaration currently being extracted.
my $parser_decl;

# Current nest level for curly brackets.
my $cur_level = 0;
# We put nest levels for curly brackets which require an ".end" mark
# into this stack.
my @ends;

my $got_erdoc_comment = 0;
my $decl_expected = 0;

# Processing curly brackets to place .open, .close and .end marks properly.
sub parser_postparse_code {
    my $code = shift @_;

    while ($code =~ /({|})/sg) {
	if ($1 eq '{') {
	    $cur_level ++;
	    print ".open\n.eob\n";
	} else {
	    print ".close\n.eob\n";
	    $cur_level --;
	    if (scalar (@ends) and
		$ends [$#ends] == $cur_level)
	    {
		print ".end\n.eob\n";
		$#ends --;
	    }
	}
    }
}

# Parsing one more line of the current declaration.
#
# We use this procedure when parsing auto-detected declarations.
sub parser_parse_decl_line {
    my $line = shift @_;

    if ($line =~ /(.*?)({|;)(.*)/) {
      # This is the last line for the declaration.

	my $decl_line = $1;
	my $decl_type = $2;
	my $the_rest = $3;

	$parser_decl .= $decl_line;

	if ($decl_type eq '{') {
=for
	    print '.', $parser_decl_type, "\n",
		  ".decl\n";
	    print $parser_decl, "\n";
	    print ".eob\n";
=cut

	    $ends [@ends] = $cur_level;
	}

      # We've extracted the declaration.

=for
	$parser_decl_type = '';
=cut
	$parser_decl = '';
	$parser_decl_mode = 0;
	$decl_expected = 0;

	parser_postparse_code ($parser_decl.$decl_type.$the_rest);
    } else {
	$parser_decl .= $line;
    }
}

sub parse_code {
    my $code = shift @_;

    while () {
	if (not $code =~ /(.*\n)/mgc) {
	  # We've got an incomplete line.
	  # Remembering it for further invocations of parse_code().
	    $code =~ /(.*)/mgc;
	    $parser_incomplete_line .= $1;
	    return;
	}

	my $line = $1;

	$line = $parser_incomplete_line.$line;
	$parser_incomplete_line = '';

	if ($parser_decl_mode) {
	  # We're in the process of collecting code for a declaration.
	    parser_parse_decl_line ($line);
	} else {
=for
	    if (($got_erdoc_comment and
		 $line =~ /^\s*(?:template\s+<[^<>]+>)?\s*(class|struct|union|enum)\s/gc) or
		 $line =~ /^\s*(namespace)\s/gc)
	    {
		$parser_decl_type = $1;
		$parser_decl_mode = 1;
		parser_parse_decl_line ($line);
	    } els
=cut
	    if ($got_erdoc_comment and
		$decl_expected)
	    {
		$parser_decl_mode = 1;
		parser_parse_decl_line ($line);
	    } elsif ($line =~ /^\s*using\s+namespace\s+(\w+)/gc) {
		my $namespace = $1;
		print ".using $namespace\n.eob\n";
		parser_postparse_code ($line);
	    } else {
		if ($line =~ /^\s*template\s+<[^<>]+>/) {
		    # This is an ad-hoc rule.
		    # Here, an erdoc comment must be present a line before
		    # a class/struct/.. declaration. However, if there
		    # was a line with a template declaration, then
		    # an erdoc comment may be _two_ lines before
		    # a class/struct/.. declaration.
		    $got_erdoc_comment ++;
		}

		parser_postparse_code ($line);
	    }
	}

	$got_erdoc_comment -- if $got_erdoc_comment > 0;
    }
}

# The previous line remembered for erdoc post-comments ( //< )
my $buffered_line;
# '1' if we have an unparsed remainder from the previously read line
# (no need to read in another one).
my $got_line;
# Line of input to be processed.
my $line;
while ($got_line or
       $line = <STDIN>)
{
    $got_line = 0;
# TODO Make this a command-line option (enabling '//' comments).
    if ($line =~ /^(.*?)(?:(\/\*|\/\/)([\*<>mcnesudt])?(.*))?\z/s) {
#    if ($line =~ /^(.*?)(?:(\/\*)([\*<>mcnesudt])?(.*))?\z/s) {
	my $code = $1;
	my $comment_type = $2;
	my $block_type = $3;
	$line = $4;

	# If the 'code' part is not empty, then we remember it for post-comments.
	$buffered_line = $code if $code and $code =~ /[^\s]+/;
	# Leaving only the last line.
	$buffered_line =~ s/.*\n(.*)$/\1/s;

	parse_code ($code);

	next if not $comment_type;

	if ($code =~ /"/) {
	  # This is a hack to avoid parsing string literals.

	  # TODO string literals should be cut out, then parse_code() should be called.

# TEST
#	    next;
	}

	# TODO Make this a command-line option (enabling '//' comments).
	if ($comment_type eq '//') {
	  # Cutting out the comment.
	    $line =~ /.*?$(.*)/ms;
	    $line = $1;
	    $got_line = 1;
	    next;
	}

      # We've got a comment.

	if (not $block_type) {
	  # This is not an erdoc comment. We're just skipping it.

	    # Single-line comment, and we've already read in the line.
	    # FIXME 'line' should be stripped till the first newline.
	    next if $comment_type eq '//';

	  # $comment_type is '/*', so this is a multiline comment.

	    while ($line) {
		if ($line =~ /\*\/(.*)/s) {
		  # End of multiline comment.

		    # Remembering the remainder of the line
		    # for the next iteration.
		    $line = $1;
		    $got_line = 1;
		    last;
		}

		$line = <STDIN>;
	    }

	  # We've skipped a non-erdoc comment.
	    next;
	}

      # We've got an erdoc comment.

	# ...this is somehow related to the relative position
	# of the declaration that corresponds to the erdoc comment.
	$got_erdoc_comment = 2;

      # Collecting contents of an erdoc block.

	my $block_data;

	if ($comment_type eq '//') {
	    $block_data = $line;
	    $line = "\n";
	} else {
	    while ($line) {
		$line =~ /^\s*(.*?)(?:(\*\/)(.*))?\z/s;
		# part of the line containing erDoc code
		my $line_data = $1;
		# end of block
		my $eob = $2;
		$line = $3;

		# Stripping whitespace / comment symbols at the front of the line.
		$line_data =~ s/^[ \t]*(?:\*|(\^|v))?[ \t]*//mg;
		# '^' and 'v' may be used to strip extra newlines.
		# FIXME: The checks for '^' and 'v' do not always work.
		if ($1 eq '^') {
		    print STDERR "STRIPPING NEWLINE ^\n";
		    $block_data =~ s/\n$//s;
		} elsif ($1 eq 'v') {
		    print STDERR "STRIPPING NEWLINE v\n";
		    $line_data  =~ s/\n$//s;
		}

		$block_data .= $line_data;

		last if $eob;
		$line = <STDIN>;
	    }
	}

	$block_data .= "\n";

      # We've collected the contents of an erdoc block.

	# TODO <> - "decl specifiers", separate
	if ($block_type eq '>') {
	    # one-line postfix declaration
	    print ".data\n";
	    print $block_data;
	    my $decl_line;
	    while ($decl_line = <STDIN>) {
		$line .= $decl_line;
		last if $decl_line =~ /[^\s]+/;
	    }
	    print ".decl\n$decl_line\n";
	    print ".eob\n";
	} elsif ($block_type eq '<') {
	    # one-line prefix declaration
	    print ".rdata\n";
	    print $block_data;
	    print ".decl\n$buffered_line\n";
	    print ".eob\n";
	} elsif ($block_type eq 'm') {
	    print ".method\n";
	    print $block_data;
	    $line .= extract_decl ($line);
	    print ".eob\n";
	} elsif ($block_type eq 'c') {
	    print ".class\n";
	    print $block_data;
	    $line .= extract_decl ($line);
	    print ".eob\n";
=for
	    print ".end\n.eob\n";
=cut

	    $decl_expected = 1;
	} elsif ($block_type eq 'n') {
	    print ".namespace\n";
	    print $block_data;
	    $line .= extract_decl ($line);
	    print ".eob\n";
	    print ".end\n.eob\n";
	} elsif ($block_type eq 'e') {
	    print ".enum\n";
	    print $block_data;
	    $line .= extract_decl ($line);
	    print ".eob\n";
=for
	    print ".end\n.eob\n";
=cut

	    $decl_expected = 1;
	} elsif ($block_type eq 's') {
	    print ".struct\n";
	    print $block_data;
	    $line .= extract_decl ($line);
	    print ".eob\n";
=for
	    print ".end\n.eob\n";
=cut

	    $decl_expected = 1;
	} elsif ($block_type eq 'u') {
	    print ".union\n";
	    print $block_data;
	    $line .= extract_decl ($line);
	    print ".eob\n";
=for
	    print ".end\n.eob\n";
=cut

	    $decl_expected = 1;
	} elsif ($block_type eq 'd') {
	    print ".data\n";
	    print $block_data;
	    $line .= extract_decl ($line);
	    print ".eob\n";
	} elsif ($block_type eq 't') {
	    print ".type_member\n";
	    print $block_data;
	    $line .= extract_decl ($line, 1);
	    print ".eob\n";
#	} elsif ($block_type eq '.') {
#	    print ".end\n";
#	    print "\n]\n";
	} else {
	    print $block_data;
	    print ".eob\n";
	}

	$got_line = 1 if $line;
    }
}

# Simulating end-of-line to process remembered incomplete line
# (if there is one).
parse_code ("\n");


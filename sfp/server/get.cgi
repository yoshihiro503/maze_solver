#!/usr/bin/perl
use CGI;
use DBI;

print "Content-type: text/html\n\n";
print "<meta http-equiv='Content-Type' content='text/html; charset=euc-jp'>\n";
print "BEGINCGI\n";

eval {
    $q = new CGI;
    $id = $q->param('id');
    $tblname = $q->param('tbl_id');

    $user = 'net';
    $passwd = 'ppp';
    $db = DBI->connect('DBI:mysql:net:localhost', $user, $passwd);
    $sth = $db->prepare("SELECT * FROM $tblname WHERE id = '$id'");

    $sth->execute;

    $num_rows = $sth->rows;

    for ($i=0; $i<$num_rows; $i++) {
	@a = $sth->fetchrow_array;
	print "$a[0],$a[1],$a[2]\n";
    }

    $sth->finish;
    $db->disconnect;
};

if( $@ ){  # $@ にエラーメッセージが入っている
    print "catch!! $@\n";
    $ans = 0;
}

print "$ans";
print "ENDCGI\n";

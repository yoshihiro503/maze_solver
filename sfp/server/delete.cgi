#!/usr/bin/perl
use CGI;
use DBI;

print "Content-type: text/html\n\n";
print "<meta http-equiv='Content-Type' content='text/html; charset=euc-jp'>";
print "Hello, Perl CGI!";

eval {
    $q = new CGI;
    $id = $q->param('id');
    $tblname = $q->param('tbl_id');

    $user = 'net';
    $passwd = 'ppp';
    print " conn ";
    $db = DBI->connect('DBI:mysql:net:localhost', $user, $passwd);
    $sth = $db->prepare("DELETE FROM $tblname WHERE id = '$id'");
    print " exec ";
    $sth->execute;
    print " finish ";
    $sth->finish;
    $db->disconnect;
};

if( $@ ){  # $@ にエラーメッセージが入っている
    print "catch!! $@\n";
    $ans = 0;
}

print "$ans\n";

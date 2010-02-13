#!/usr/bin/perl
use CGI;
use DBI;

print "Content-type: text/html\n\n";
print "<meta http-equiv='Content-Type' content='text/html; charset=euc-jp'>";
print "Hello, Perl CGI!";

eval {
    $q = new CGI;
    $id = $q->param('id');
    $body = $q->param('body');
    $date = get_time();
    $tblname = $q->param('tbl_id');

    $user = 'net';
    $passwd = 'ppp';
    print " conn ";
    $db = DBI->connect('DBI:mysql:net:localhost', $user, $passwd);
    $sth = $db->prepare("INSERT INTO $tblname VALUES ('$id','$date','$body')");
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

sub get_time {
#time関数で得られた数値をlocaltime関数で整形し各変数に入れる
    ($sec,$min,$hour,$mday,$mon,$year,$wno) = localtime(time);

#曜日は 0〜6 の値が返されるので配列で扱う
    @wdays = ('SUN','MON','TUE','WED','THU','FRI','SAT');

#返された値をさらに sprintf関数で整形し、書式を読みやすいようにする。
    return sprintf("%04d-%02d-%02d %02d:%02d:%02d",$year+1900,$mon+1,$mday,$hour,$min,$sec);
}

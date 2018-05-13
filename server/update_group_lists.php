<?php
	$conn = new mysqli("localhost", "root", "t8zDCrHPvJnm", "obumbl");
	if ($conn->connect_error)
		die("Connection failed: " . $conn->connect_error);

	if ($conn->query("REPLACE INTO `groups` (`group_id`,`group_blacklist`,`invited_groups_list`,`received_invites_list`) VALUES (".$_POST['group_id'].",\"".$_POST['group_blacklist']."\",\"".$_POST['invited_groups_list']."\",\"".$_POST['received_invites_list']."\");")) {
		echo "1";
	} else {
		echo "-1";
	}

	$conn->close();
?>
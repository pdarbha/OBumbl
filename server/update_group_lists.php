<?php
	$conn = new mysqli("localhost", "root", "t8zDCrHPvJnm", "obumbl");
	if ($conn->connect_error)
		die("Connection failed: " . $conn->connect_error);

	if ($conn->query("UPDATE `groups` SET `group_blacklist`= \"".$_POST['group_blacklist']."\", `invited_groups_list` = \"".$_POST['invited_groups_list']."\", `received_invites_list` = \"".$_POST['received_invites_list']."\" WHERE `group_id` = ".$_POST['group_id'].";")) {
		echo "1";
	} else {
		echo "-1";
	}

	$conn->close();
?>
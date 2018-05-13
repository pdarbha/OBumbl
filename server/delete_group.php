<?php
	$conn = new mysqli("localhost", "root", "t8zDCrHPvJnm", "obumbl");
	if ($conn->connect_error)
		die("Connection failed: " . $conn->connect_error);

	if ($conn->query("DELETE FROM `groups` WHERE `group_id`=".$_POST['group_id'].";")) {
		echo "1";
	} else {
		echo "-1";
	}

	$conn->close();
?>
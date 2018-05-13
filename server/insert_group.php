<?php
	$conn = new mysqli("localhost", "root", "t8zDCrHPvJnm", "obumbl");
	if ($conn->connect_error)
		die("Connection failed: " . $conn->connect_error);

	if ($conn->query("INSERT INTO `groups` (`user_id_list`,`purpose`,`size`,`range_min`,`range_max`,`schedule`,`group_blacklist`,`invited_groups_list`,`received_invites_list`) VALUES (\"".$_POST['user_id_list']."\",\"".$_POST['purpose']."\",\"".$_POST['size']."\",\"".$_POST['range_min']."\",\"".$_POST['range_max']."\",\"".$_POST['schedule']."\",\"".$_POST['group_blacklist']."\",\"".$_POST['invited_groups_list']."\",\"".$_POST['received_invites_list']."\");")) {
		$result = $conn->query("SELECT LAST_INSERT_ID();");
		$row = $result->fetch_assoc();
		echo $row["LAST_INSERT_ID()"];
	} else {
		echo "-1";
	}

	$conn->close();
?>
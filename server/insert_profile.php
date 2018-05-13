<?php
	$conn = new mysqli("localhost", "root", "t8zDCrHPvJnm", "obumbl");
	if ($conn->connect_error)
		die("Connection failed: " . $conn->connect_error);

	if ($conn->query("REPLACE INTO `profiles` (`user_id`,`name`,`photo`,`school`,`group_list`,`description`,`interest_list`,`experience`,`role`,`looking_for`,`github_url`,`email`) VALUES (".$_POST['user_id'].",\"".$_POST['name']."\",\"".$_POST['photo']."\",\"".$_POST['school']."\",\"".$_POST['group_list']."\",\"".$_POST['description']."\",\"".$_POST['interest_list']."\",\"".$_POST['experience']."\",\"".$_POST['role']."\",\"".$_POST['looking_for']."\",\"".$_POST['github_url']."\",\"".$_POST['email']."\");")) {
		echo "1";
	} else {
		echo "-1";
	}

	$conn->close();
?>
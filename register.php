<?php
	$conn = new mysqli("localhost", "root", "t8zDCrHPvJnm", "obumbl");
	if ($conn->connect_error)
		die("Connection failed: " . $conn->connect_error);

	if ($result = $conn->query("INSERT INTO `credentials` (`username`,`password`) VALUES (\"".$_POST['username']."\",\"".$_POST['password']."\")")) {
		$id = $conn->query("SELECT `user_id` FROM `credentials` WHERE `username` = \"".$_POST['username']."\" AND `password` = \"".$_POST['password']."\";");
		$row = $id->fetch_assoc();
		echo $row['user_id'];
	} else {
		echo "-1";
	}

	$conn->close();
?>
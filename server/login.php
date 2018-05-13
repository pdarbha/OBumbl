<?php
	$conn = new mysqli("localhost", "root", "t8zDCrHPvJnm", "obumbl");
	if ($conn->connect_error)
		die("Connection failed: " . $conn->connect_error);

	if ($result = $conn->query("SELECT `user_id` FROM `credentials` WHERE `username` = \"".$_POST['username']."\" AND `password` = \"".$_POST['password']."\";")) {
		if($result->num_rows == 1) {
			$row = $result->fetch_assoc();
			echo $row['user_id'];
		} else {
			echo "-1";
		}
		$result->close();
	} else {
		echo "-1";
	}

	$conn->close();
?>
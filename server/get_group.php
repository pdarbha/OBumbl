<?php
	$conn = new mysqli("localhost", "root", "t8zDCrHPvJnm", "obumbl");
	if ($conn->connect_error)
		die("Connection failed: " . $conn->connect_error);

	if ($result = $conn->query("SELECT * FROM `groups` WHERE `group_id` = ".$_GET['group_id'].";")) {
		if($result->num_rows == 1) {
			echo json_encode($result->fetch_assoc());
		} else {
			echo "-1";
		}
		$result->close();
	} else {
		echo "-1";
	}

	$conn->close();
?>
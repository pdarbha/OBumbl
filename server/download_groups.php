<?php
	$conn = new mysqli("localhost", "root", "t8zDCrHPvJnm", "obumbl");
	if ($conn->connect_error)
		die("Connection failed: " . $conn->connect_error);

	if ($result = $conn->query("SELECT * FROM `groups` WHERE `purpose`=\"".$_GET['purpose']."\"")) {
		$rows = array();
		while($r = mysqli_fetch_assoc($result)) {
			$rows[] = $r;
		}
		echo json_encode($rows);
	}

	$conn->close();
?>
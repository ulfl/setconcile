resource "aws_vpc" "default" {
    cidr_block = "10.0.0.0/16"
    enable_dns_hostnames = true
    tags { Name = "replication-test" }
}

resource "aws_internet_gateway" "default" {
    vpc_id = "${aws_vpc.default.id}"
    tags { Name = "replication-test" }
}

resource "aws_security_group" "my_sec_group" {
    ingress {
        from_port = 22
        to_port = 22
        protocol = "tcp"
        cidr_blocks = ["0.0.0.0/0"]
    }

    ingress {
        from_port = 7363
        to_port = 7363
        protocol = "tcp"
        cidr_blocks = ["0.0.0.0/0"]
    }

    egress {
        from_port = 0
        to_port = 65535
        protocol = "tcp"
        cidr_blocks = ["0.0.0.0/0"]
    }

    vpc_id = "${aws_vpc.default.id}"

    tags { Name = "replication-test" }
}

resource "aws_subnet" "public_subnet" {
    vpc_id = "${aws_vpc.default.id}"
    cidr_block = "10.0.1.0/24"
    tags { Name = "replication-test" }
}

resource "aws_route_table" "route_table_public" {
    vpc_id = "${aws_vpc.default.id}"
    route {
        cidr_block = "0.0.0.0/0"
        gateway_id = "${aws_internet_gateway.default.id}"
    }
    tags { Name = "replication-test" }
}

resource "aws_route_table_association" "route_table_association" {
    subnet_id = "${aws_subnet.public_subnet.id}"
    route_table_id = "${aws_route_table.route_table_public.id}"
}

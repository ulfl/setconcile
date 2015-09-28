variable "access_key" {}
variable "secret_key" {}
variable "key_name" {}
variable "key_file" {}
variable "region" { }
variable "amis" {
    default = {
        us-east-1 = "ami-51ef9534" # f862c1e-1M-0.1-4096-nodeb
        eu-west-1 = "ami-9bc8e3ec" # f862c1e-1M-0.1-4096-nodea
#        us-east-1 = "ami-2dcf7b46" # ubuntu 14.04.3
#        eu-west-1 = "ami-92401ce5" # ubuntu 14.04.3
#        eu-west-1 = "ami-c796beb0" # 12d3e70-1M-0.001-4096-nodea.
#        us-east-1 = "ami-4998e32c" # 12d3e70-1M-0.001-4096-nodeb.
    }
}

provider "aws" {
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    region = "${var.region}"
}

resource "aws_instance" "replication_node" {
    ami = "${lookup(var.amis, var.region)}"
    instance_type = "m4.large"
    key_name = "${var.key_name}"
    security_groups = ["${aws_security_group.my_sec_group.id}"]
    subnet_id = "${aws_subnet.public_subnet.id}"
    associate_public_ip_address = true
    tags { Name = "replication-test" }

    provisioner "file" {
        connection {user = "ubuntu" key_file = "${var.key_file}"}
        source = "${path.module}/riak.patch"
        destination = "riak.patch"
    }

    # provisioner "remote-exec" {
    #     connection {user = "ubuntu" key_file = "${var.key_file}"}
    #     script = "${path.module}/setup.sh"
    # }

    # Run separately so that the limits.conf changes are in affect.
    provisioner "remote-exec" {
        connection {user = "ubuntu" key_file = "${var.key_file}"}
        script = "${path.module}/start_riak.sh"
    }
}

output "ip" {
    value = "${aws_instance.replication_node.public_ip}"
}

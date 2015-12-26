variable "access_key" {}
variable "secret_key" {}
variable "key_name" {}
variable "key_file" {}
variable "region" { }
variable "amis" {
    default = {
#        eu-west-1 = "ami-8e67cbfd" # 30f5ed9-1M-0.001-1024-nodea
#        us-east-1 = "ami-fb94c491" # 30f5ed9-1M-0.001-1024-nodeb
        eu-west-1 = "ami-efef439c" # 7ed5029-5M-0.001-1024-nodea
        us-east-1 = "ami-185d0c72" # 7ed5029-5M-0.001-1024-nodeb
#        eu-west-1 = "ami-5104a922" # 1aea723-1M-0.001-1024-nodea
#        us-east-1 = "ami-6e025304" # 1aea723-1M-0.001-1024-nodeb
#        eu-west-1 = "ami-105df063" # a3addc1-1M-0.001-1024-nodea
#        us-east-1 = "ami-d0a5f3ba" # a3addc1-1M-0.001-1024-nodeb
#        eu-west-1 = "ami-92401ce5" # ubuntu 14.04.3.
#        us-east-1 = "ami-2dcf7b46" # ubuntu 14.04.3.
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

    root_block_device {
        volume_type = "gp2"
        volume_size = 500
    }
    
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

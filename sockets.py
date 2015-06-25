from fabric.api import task, local

@task
def run_send( host = 'localhost', port = 1338):
	port = int(port)
	local('make sockets')
	local('rlwrap java SocketSend {0} {1}'.format(host, port))

@task
def run_receive( port = 1339):
	port = int(port)
	local('make sockets')
	local('rlwrap java SocketReceive {0}'.format(port))

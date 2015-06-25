import java.io.*;
import java.net.*;
import java.util.*;

public class SocketReceive {

	public static void main(String[] args) throws UnknownHostException {
		int portNumber = Integer.parseInt( args[0] );
		boolean running = true;
		
		try ( DatagramSocket socket = new DatagramSocket( portNumber ); ) {
			while(running) {
				byte[] buf = new byte[256];
				
				// receive request
                DatagramPacket packet = new DatagramPacket(buf, buf.length);
                socket.receive(packet);
                String received = new String(packet.getData(), 0, packet.getLength());
                
                System.out.println("Got response: " + received);
                if(received.equals("exit")) {
					running = false;
				}
			}
		} catch (SocketException e) {
			System.err.println("Could not bind to port " + portNumber );
			System.exit(1);
		} catch (IOException e) {
			System.err.println("I/O Error");
			e.printStackTrace();
			System.exit(2);
		} 
	}
}
	

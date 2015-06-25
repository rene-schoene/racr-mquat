import java.io.*;
import java.net.*;
import java.util.*;

public class SocketSend {

	public static void main(String[] args) throws UnknownHostException {
		String hostName = args[0];
		int portNumber = Integer.parseInt( args[1] );
		InetAddress ia = InetAddress.getByName( hostName );
		
		try (
			DatagramSocket toSocket = new DatagramSocket();
			BufferedReader stdIn = new BufferedReader(new InputStreamReader(System.in))
		) {
			String userInput;
			while ((userInput = stdIn.readLine()) != null) {
				//request %d %s %lf %lf %lf -> &work_id, objective, &max_e, &max_rt, &min_p
				byte[] raw = userInput.getBytes();
				System.out.println("using datagram");
				userInput = userInput.substring(3);
				DatagramPacket packet = new DatagramPacket( raw, raw.length, ia, portNumber );
				toSocket.send( packet );
				System.out.println("sent");
			}
		} catch (UnknownHostException e) {
			System.err.println("Don't know about host " + hostName);
			System.exit(1);
		} catch (IOException e) {
			System.err.println("Couldn't get I/O for the connection to " + hostName);
			e.printStackTrace();
			System.exit(2);
		} 
	}
}
	

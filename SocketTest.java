import java.io.*;
import java.net.*;
import java.util.*;

public class SocketTest {

	public static void main(String[] args) throws UnknownHostException {
		String hostName = args[0];
		int portNumber = Integer.parseInt( args[1] );
		InetAddress ia = InetAddress.getByName( hostName );
		
		try (
			DatagramSocket toSocket = new DatagramSocket();
			Socket serverSocket = new Socket( hostName, portNumber );
			PrintWriter out = new PrintWriter(serverSocket.getOutputStream(), true);
			BufferedReader in = new BufferedReader(
				new InputStreamReader(serverSocket.getInputStream()));
			BufferedReader stdIn = new BufferedReader(new InputStreamReader(System.in))
		) {
			String userInput;
			while ((userInput = stdIn.readLine()) != null) {
				//request %d %s %lf %lf %lf -> &work_id, objective, &max_e, &max_rt, &min_p
				byte[] raw = userInput.getBytes();
				if( userInput.substring(0,3).equals("dg ") ) {
					System.out.println("using datagram");
					userInput = userInput.substring(3);
					DatagramPacket packet = new DatagramPacket( raw, raw.length, ia, portNumber );
					toSocket.send( packet );
					System.out.println("sent");
				} else {
					out.println(userInput);
					System.out.println("echo: " + in.readLine());
				}
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
	

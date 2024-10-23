#include <iostream>
#include <mavlink.h>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include <arpa/inet.h>

#define UDP_PORT 8888 // Use this port for MAVLink communication
#define TARGET_SYSTEM 1 // Usually the system ID of the drone
#define TARGET_COMPONENT 1 // Usually the component ID (like the autopilot)

int main() {
    int sockfd;
    struct sockaddr_in server_addr;
    uint8_t buffer[MAVLINK_MAX_PACKET_LENGTH];

    // Create a UDP socket
    sockfd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sockfd < 0) {
        std::cerr << "Failed to create socket\n";
        return 1;
    }

    // Set up the server address
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(UDP_PORT);
    server_addr.sin_addr.s_addr = inet_addr("127.0.0.1"); // Localhost

    // Prepare the MAVLink takeoff command
    mavlink_message_t msg;
    mavlink_msg_command_long_pack(1, 200, &msg, TARGET_SYSTEM, TARGET_COMPONENT,
                                   MAV_CMD_NAV_TAKEOFF, 0, 0, 0, 0, 10, 0); // Altitude 10 meters

    // Convert the MAVLink message to a sendable buffer
    unsigned len = mavlink_msg_to_send_buffer(buffer, &msg);
    
    // Send the message
    if (sendto(sockfd, buffer, len, 0, (struct sockaddr*)&server_addr, sizeof(server_addr)) < 0) {
        std::cerr << "Failed to send message\n";
        close(sockfd);
        return 1;
    }

    std::cout << "Takeoff command sent!\n";

    // Clean up and close the socket
    close(sockfd);
    return 0;
}

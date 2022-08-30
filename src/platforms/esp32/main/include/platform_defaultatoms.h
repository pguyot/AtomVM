/*
 * This file is part of AtomVM.
 *
 * Copyright 2019 Davide Bettio <davide@uninstall.it>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#ifndef _PLATFORM_DEFAULTATOMS_H_
#define _PLATFORM_DEFAULTATOMS_H_

#include "defaultatoms.h"

#define SET_LEVEL_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 0)
#define READ_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 1)
#define INPUT_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 2)
#define OUTPUT_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 3)
#define SET_DIRECTION_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 4)
#define SET_INT_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 5)
#define GPIO_INTERRUPT_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 6)
#define RISING_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 7)
#define FALLING_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 8)
#define BOTH_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 9)
#define LOW_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 10)
#define HIGH_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 11)
#define REMOVE_INT_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 12)
#define OUTPUT_OD_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 13)

#define ESP32_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 14)

#define SOCKET_ATOMS_BASE_INDEX (PLATFORM_ATOMS_BASE_INDEX + 15)
#define PROTO_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 0)
#define UDP_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 1)
#define TCP_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 2)
#define SOCKET_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 3)
#define FCNTL_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 4)
#define BIND_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 5)
#define GETSOCKNAME_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 6)
#define RECVFROM_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 7)
#define SENDTO_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 8)
#define ADDRESS_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 9)
#define PORT_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 10)
#define CONTROLLING_PROCESS_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 11)
#define BINARY_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 12)
#define ACTIVE_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 13)
#define BUFFER_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 14)
#define CONNECT_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 15)
#define SEND_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 16)
#define TCP_CLOSED_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 17)
#define RECV_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 18)
#define LISTEN_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 19)
#define BACKLOG_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 20)
#define ACCEPT_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 21)
#define FD_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 22)

#define INIT_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 23)
#define CLOSE_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 24)
#define GET_PORT_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 25)
#define SOCKNAME_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 26)
#define PEERNAME_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 27)
#define NOT_OWNER_ATOM_INDEX (SOCKET_ATOMS_BASE_INDEX + 28)

#define NETWORK_ATOMS_BASE_INDEX (SOCKET_ATOMS_BASE_INDEX + 29)
#define STA_ATOM_INDEX (NETWORK_ATOMS_BASE_INDEX + 0)
#define SSID_ATOM_INDEX (NETWORK_ATOMS_BASE_INDEX + 1)
#define PSK_ATOM_INDEX (NETWORK_ATOMS_BASE_INDEX + 2)
#define SNTP_ATOM_INDEX (NETWORK_ATOMS_BASE_INDEX + 3)
#define STA_GOT_IP_ATOM_INDEX (NETWORK_ATOMS_BASE_INDEX + 4)
#define STA_CONNECTED_ATOM_INDEX (NETWORK_ATOMS_BASE_INDEX + 5)
#define STA_DISCONNECTED_ATOM_INDEX (NETWORK_ATOMS_BASE_INDEX + 6)
#define STA_DHCP_HOSTNAME_INDEX (NETWORK_ATOMS_BASE_INDEX + 7)
#define AP_ATOM_INDEX (NETWORK_ATOMS_BASE_INDEX + 8)
#define SSID_HIDDEN_ATOM_INDEX (NETWORK_ATOMS_BASE_INDEX + 9)
#define MAX_CONNECTIONS_ATOM_INDEX (NETWORK_ATOMS_BASE_INDEX + 10)
#define AP_STARTED_ATOM_INDEX (NETWORK_ATOMS_BASE_INDEX + 11)
#define AP_STA_CONNECTED_ATOM_INDEX (NETWORK_ATOMS_BASE_INDEX + 12)
#define AP_STA_DISCONNECTED_ATOM_INDEX (NETWORK_ATOMS_BASE_INDEX + 13)
#define AP_STA_IP_ASSIGNED_ATOM_INDEX (NETWORK_ATOMS_BASE_INDEX + 14)
#define HOST_ATOM_INDEX (NETWORK_ATOMS_BASE_INDEX + 15)

#define SPIDRIVER_ATOMS_BASE_INDEX (NETWORK_ATOMS_BASE_INDEX + 16)
#define BUS_CONFIG_ATOM_INDEX (SPIDRIVER_ATOMS_BASE_INDEX + 0)
#define MISO_IO_NUM_ATOM_INDEX (SPIDRIVER_ATOMS_BASE_INDEX + 1)
#define MOSI_IO_NUM_ATOM_INDEX (SPIDRIVER_ATOMS_BASE_INDEX + 2)
#define SCLK_IO_NUM_ATOM_INDEX (SPIDRIVER_ATOMS_BASE_INDEX + 3)
#define SPI_PERIPHERAL_ATOM_INDEX (SPIDRIVER_ATOMS_BASE_INDEX + 4)
#define HSPI_ATOM_INDEX (SPIDRIVER_ATOMS_BASE_INDEX + 5)
#define VSPI_ATOM_INDEX (SPIDRIVER_ATOMS_BASE_INDEX + 6)
#define DEVICE_CONFIG_ATOM_INDEX (SPIDRIVER_ATOMS_BASE_INDEX + 7)
#define SPI_CLOCK_HZ_ATOM_INDEX (SPIDRIVER_ATOMS_BASE_INDEX + 8)
#define SPI_MODE_ATOM_INDEX (SPIDRIVER_ATOMS_BASE_INDEX + 9)
#define SPI_CS_IO_NUM_ATOM_INDEX (SPIDRIVER_ATOMS_BASE_INDEX + 10)
#define ADDRESS_LEN_BITS_ATOM_INDEX (SPIDRIVER_ATOMS_BASE_INDEX + 11)
#define COMMAND_LEN_BITS_ATOM_INDEX (SPIDRIVER_ATOMS_BASE_INDEX + 12)
#define WRITE_READ_ATOM_INDEX (SPIDRIVER_ATOMS_BASE_INDEX + 13)
#define READ_AT_ATOM_INDEX (SPIDRIVER_ATOMS_BASE_INDEX + 14)
#define WRITE_AT_ATOM_INDEX (SPIDRIVER_ATOMS_BASE_INDEX + 15)

#define I2CDRIVER_ATOMS_BASE_INDEX (WRITE_AT_ATOM_INDEX + 1)
#define BEGIN_TRANSMISSION_ATOM_INDEX (I2CDRIVER_ATOMS_BASE_INDEX + 0)
#define END_TRANSMISSION_ATOM_INDEX (I2CDRIVER_ATOMS_BASE_INDEX + 1)
#define WRITE_BYTE_ATOM_INDEX (I2CDRIVER_ATOMS_BASE_INDEX + 2)
#define READ_BYTES_ATOM_INDEX (I2CDRIVER_ATOMS_BASE_INDEX + 3)
#define WRITE_BYTES_ATOM_INDEX (I2CDRIVER_ATOMS_BASE_INDEX + 4)
#define SCL_IO_NUM_ATOM_INDEX (I2CDRIVER_ATOMS_BASE_INDEX + 5)
#define SDA_IO_NUM_ATOM_INDEX (I2CDRIVER_ATOMS_BASE_INDEX + 6)
#define I2C_CLOCK_HZ_ATOM_INDEX (I2CDRIVER_ATOMS_BASE_INDEX + 7)

#define UART_ATOMS_BASE_INDEX (I2C_CLOCK_HZ_ATOM_INDEX + 1)
#define NAME_ATOM_INDEX (UART_ATOMS_BASE_INDEX + 0)
#define SPEED_ATOM_INDEX (UART_ATOMS_BASE_INDEX + 1)
#define WRITE_ATOM_INDEX (UART_ATOMS_BASE_INDEX + 2)
#define DATA_BITS_ATOM_INDEX (UART_ATOMS_BASE_INDEX + 3)
#define STOP_BITS_ATOM_INDEX (UART_ATOMS_BASE_INDEX + 4)
#define FLOW_CONTROL_ATOM_INDEX (UART_ATOMS_BASE_INDEX + 5)
#define HARDWARE_ATOM_INDEX (UART_ATOMS_BASE_INDEX + 6)
#define SOFTWARE_ATOM_INDEX (UART_ATOMS_BASE_INDEX + 7)
#define PARITY_ATOM_INDEX (UART_ATOMS_BASE_INDEX + 8)
#define EVEN_ATOM_INDEX (UART_ATOMS_BASE_INDEX + 9)
#define ODD_ATOM_INDEX (UART_ATOMS_BASE_INDEX + 10)
#define TX_PIN_ATOM_INDEX (UART_ATOMS_BASE_INDEX + 11)
#define RX_PIN_ATOM_INDEX (UART_ATOMS_BASE_INDEX + 12)
#define RTS_PIN_ATOM_INDEX (UART_ATOMS_BASE_INDEX + 13)
#define CTS_PIN_ATOM_INDEX (UART_ATOMS_BASE_INDEX + 14)
#define DEFAULT_ATOM_INDEX (UART_ATOMS_BASE_INDEX + 15)
#define EVENT_QUEUE_LEN_ATOM_INDEX (UART_ATOMS_BASE_INDEX + 16)

#define SET_LEVEL_ATOM TERM_FROM_ATOM_INDEX(SET_LEVEL_ATOM_INDEX)
#define READ_ATOM TERM_FROM_ATOM_INDEX(READ_ATOM_INDEX)
#define INPUT_ATOM TERM_FROM_ATOM_INDEX(INPUT_ATOM_INDEX)
#define OUTPUT_ATOM TERM_FROM_ATOM_INDEX(OUTPUT_ATOM_INDEX)
#define SET_DIRECTION_ATOM TERM_FROM_ATOM_INDEX(SET_DIRECTION_ATOM_INDEX)
#define SET_INT_ATOM TERM_FROM_ATOM_INDEX(SET_INT_ATOM_INDEX)
#define GPIO_INTERRUPT_ATOM TERM_FROM_ATOM_INDEX(GPIO_INTERRUPT_ATOM_INDEX)
#define RISING_ATOM TERM_FROM_ATOM_INDEX(RISING_ATOM_INDEX)
#define FALLING_ATOM TERM_FROM_ATOM_INDEX(FALLING_ATOM_INDEX)
#define BOTH_ATOM TERM_FROM_ATOM_INDEX(BOTH_ATOM_INDEX)
#define LOW_ATOM TERM_FROM_ATOM_INDEX(LOW_ATOM_INDEX)
#define HIGH_ATOM TERM_FROM_ATOM_INDEX(HIGH_ATOM_INDEX)
#define REMOVE_INT_ATOM TERM_FROM_ATOM_INDEX(REMOVE_INT_ATOM_INDEX)
#define OUTPUT_OD_ATOM TERM_FROM_ATOM_INDEX(OUTPUT_OD_ATOM_INDEX)

#define ESP32_ATOM TERM_FROM_ATOM_INDEX(ESP32_ATOM_INDEX)

// socket
#define PROTO_ATOM TERM_FROM_ATOM_INDEX(PROTO_ATOM_INDEX)
#define UDP_ATOM TERM_FROM_ATOM_INDEX(UDP_ATOM_INDEX)
#define TCP_ATOM TERM_FROM_ATOM_INDEX(TCP_ATOM_INDEX)
#define SOCKET_ATOM TERM_FROM_ATOM_INDEX(SOCKET_ATOM_INDEX)
#define FCNTL_ATOM TERM_FROM_ATOM_INDEX(FCNTL_ATOM_INDEX)
#define BIND_ATOM TERM_FROM_ATOM_INDEX(BIND_ATOM_INDEX)
#define GETSOCKNAME_ATOM TERM_FROM_ATOM_INDEX(GETSOCKNAME_ATOM_INDEX)
#define RECVFROM_ATOM TERM_FROM_ATOM_INDEX(RECVFROM_ATOM_INDEX)
#define SENDTO_ATOM TERM_FROM_ATOM_INDEX(SENDTO_ATOM_INDEX)
#define ADDRESS_ATOM term_from_atom_index(ADDRESS_ATOM_INDEX)
#define PORT_ATOM term_from_atom_index(PORT_ATOM_INDEX)
#define CONTROLLING_PROCESS_ATOM TERM_FROM_ATOM_INDEX(CONTROLLING_PROCESS_ATOM_INDEX)
#define BINARY_ATOM term_from_atom_index(BINARY_ATOM_INDEX)
#define ACTIVE_ATOM term_from_atom_index(ACTIVE_ATOM_INDEX)
#define BUFFER_ATOM term_from_atom_index(BUFFER_ATOM_INDEX)
#define CONNECT_ATOM term_from_atom_index(CONNECT_ATOM_INDEX)
#define SEND_ATOM TERM_FROM_ATOM_INDEX(SEND_ATOM_INDEX)
#define TCP_CLOSED_ATOM TERM_FROM_ATOM_INDEX(TCP_CLOSED_ATOM_INDEX)
#define RECV_ATOM TERM_FROM_ATOM_INDEX(RECV_ATOM_INDEX)
#define LISTEN_ATOM TERM_FROM_ATOM_INDEX(LISTEN_ATOM_INDEX)
#define BACKLOG_ATOM TERM_FROM_ATOM_INDEX(BACKLOG_ATOM_INDEX)
#define ACCEPT_ATOM TERM_FROM_ATOM_INDEX(ACCEPT_ATOM_INDEX)
#define FD_ATOM TERM_FROM_ATOM_INDEX(FD_ATOM_INDEX)

#define INIT_ATOM TERM_FROM_ATOM_INDEX(INIT_ATOM_INDEX)
#define CLOSE_ATOM TERM_FROM_ATOM_INDEX(CLOSE_ATOM_INDEX)
#define GET_PORT_ATOM TERM_FROM_ATOM_INDEX(GET_PORT_ATOM_INDEX)
#define SOCKNAME_ATOM TERM_FROM_ATOM_INDEX(SOCKNAME_ATOM_INDEX)
#define PEERNAME_ATOM TERM_FROM_ATOM_INDEX(PEERNAME_ATOM_INDEX)
#define NOT_OWNER_ATOM TERM_FROM_ATOM_INDEX(NOT_OWNER_ATOM_INDEX)

// network
#define STA_ATOM TERM_FROM_ATOM_INDEX(STA_ATOM_INDEX)
#define SSID_ATOM TERM_FROM_ATOM_INDEX(SSID_ATOM_INDEX)
#define PSK_ATOM TERM_FROM_ATOM_INDEX(PSK_ATOM_INDEX)
#define SNTP_ATOM TERM_FROM_ATOM_INDEX(SNTP_ATOM_INDEX)
#define STA_GOT_IP_ATOM TERM_FROM_ATOM_INDEX(STA_GOT_IP_ATOM_INDEX)
#define STA_CONNECTED_ATOM TERM_FROM_ATOM_INDEX(STA_CONNECTED_ATOM_INDEX)
#define STA_DISCONNECTED_ATOM TERM_FROM_ATOM_INDEX(STA_DISCONNECTED_ATOM_INDEX)
#define STA_DHCP_HOSTNAME_ATOM TERM_FROM_ATOM_INDEX(STA_DHCP_HOSTNAME_INDEX)
#define AP_ATOM TERM_FROM_ATOM_INDEX(AP_ATOM_INDEX)
#define SSID_HIDDEN_ATOM TERM_FROM_ATOM_INDEX(SSID_HIDDEN_ATOM_INDEX)
#define MAX_CONNECTIONS_ATOM TERM_FROM_ATOM_INDEX(MAX_CONNECTIONS_ATOM_INDEX)
#define AP_STARTED_ATOM TERM_FROM_ATOM_INDEX(AP_STARTED_ATOM_INDEX)
#define AP_STA_CONNECTED_ATOM TERM_FROM_ATOM_INDEX(AP_STA_CONNECTED_ATOM_INDEX)
#define AP_STA_DISCONNECTED_ATOM TERM_FROM_ATOM_INDEX(AP_STA_DISCONNECTED_ATOM_INDEX)
#define AP_STA_IP_ASSIGNED_ATOM TERM_FROM_ATOM_INDEX(AP_STA_IP_ASSIGNED_ATOM_INDEX)
#define HOST_ATOM TERM_FROM_ATOM_INDEX(HOST_ATOM_INDEX)

// spidriver
#define BUS_CONFIG_ATOM TERM_FROM_ATOM_INDEX(BUS_CONFIG_ATOM_INDEX)
#define MISO_IO_NUM_ATOM TERM_FROM_ATOM_INDEX(MISO_IO_NUM_ATOM_INDEX)
#define MOSI_IO_NUM_ATOM TERM_FROM_ATOM_INDEX(MOSI_IO_NUM_ATOM_INDEX)
#define SCLK_IO_NUM_ATOM TERM_FROM_ATOM_INDEX(SCLK_IO_NUM_ATOM_INDEX)
#define SPI_PERIPHERAL_ATOM TERM_FROM_ATOM_INDEX(SPI_PERIPHERAL_ATOM_INDEX)
#define HSPI_ATOM TERM_FROM_ATOM_INDEX(HSPI_ATOM_INDEX)
#define VSPI_ATOM TERM_FROM_ATOM_INDEX(VSPI_ATOM_INDEX)
#define DEVICE_CONFIG_ATOM TERM_FROM_ATOM_INDEX(DEVICE_CONFIG_ATOM_INDEX)
#define SPI_CLOCK_HZ_ATOM TERM_FROM_ATOM_INDEX(SPI_CLOCK_HZ_ATOM_INDEX)
#define SPI_MODE_ATOM TERM_FROM_ATOM_INDEX(SPI_MODE_ATOM_INDEX)
#define SPI_CS_IO_NUM_ATOM TERM_FROM_ATOM_INDEX(SPI_CS_IO_NUM_ATOM_INDEX)
#define ADDRESS_LEN_BITS_ATOM TERM_FROM_ATOM_INDEX(ADDRESS_LEN_BITS_ATOM_INDEX)
#define COMMAND_LEN_BITS_ATOM TERM_FROM_ATOM_INDEX(COMMAND_LEN_BITS_ATOM_INDEX)
#define WRITE_READ_ATOM TERM_FROM_ATOM_INDEX(WRITE_READ_ATOM_INDEX)
#define READ_AT_ATOM TERM_FROM_ATOM_INDEX(READ_AT_ATOM_INDEX)
#define WRITE_AT_ATOM TERM_FROM_ATOM_INDEX(WRITE_AT_ATOM_INDEX)

// i2cdriver
#define BEGIN_TRANSMISSION_ATOM TERM_FROM_ATOM_INDEX(BEGIN_TRANSMISSION_ATOM_INDEX)
#define END_TRANSMISSION_ATOM TERM_FROM_ATOM_INDEX(END_TRANSMISSION_ATOM_INDEX)
#define WRITE_BYTE_ATOM TERM_FROM_ATOM_INDEX(WRITE_BYTE_ATOM_INDEX)
#define READ_BYTES_ATOM TERM_FROM_ATOM_INDEX(READ_BYTES_ATOM_INDEX)
#define WRITE_BYTES_ATOM TERM_FROM_ATOM_INDEX(WRITE_BYTES_ATOM_INDEX)
#define SCL_IO_NUM_ATOM TERM_FROM_ATOM_INDEX(SCL_IO_NUM_ATOM_INDEX)
#define SDA_IO_NUM_ATOM TERM_FROM_ATOM_INDEX(SDA_IO_NUM_ATOM_INDEX)
#define I2C_CLOCK_HZ_ATOM TERM_FROM_ATOM_INDEX(I2C_CLOCK_HZ_ATOM_INDEX)

// uart_driver
#define NAME_ATOM TERM_FROM_ATOM_INDEX(NAME_ATOM_INDEX)
#define SPEED_ATOM TERM_FROM_ATOM_INDEX(SPEED_ATOM_INDEX)
#define WRITE_ATOM TERM_FROM_ATOM_INDEX(WRITE_ATOM_INDEX)
#define DATA_BITS_ATOM TERM_FROM_ATOM_INDEX(DATA_BITS_ATOM_INDEX)
#define STOP_BITS_ATOM TERM_FROM_ATOM_INDEX(STOP_BITS_ATOM_INDEX)
#define FLOW_CONTROL_ATOM TERM_FROM_ATOM_INDEX(FLOW_CONTROL_ATOM_INDEX)
#define HARDWARE_ATOM TERM_FROM_ATOM_INDEX(HARDWARE_ATOM_INDEX)
#define SOFTWARE_ATOM TERM_FROM_ATOM_INDEX(SOFTWARE_ATOM_INDEX)
#define PARITY_ATOM TERM_FROM_ATOM_INDEX(PARITY_ATOM_INDEX)
#define EVEN_ATOM TERM_FROM_ATOM_INDEX(EVEN_ATOM_INDEX)
#define ODD_ATOM TERM_FROM_ATOM_INDEX(ODD_ATOM_INDEX)
#define TX_PIN_ATOM TERM_FROM_ATOM_INDEX(TX_PIN_ATOM_INDEX)
#define RX_PIN_ATOM TERM_FROM_ATOM_INDEX(RX_PIN_ATOM_INDEX)
#define RTS_PIN_ATOM TERM_FROM_ATOM_INDEX(RTS_PIN_ATOM_INDEX)
#define CTS_PIN_ATOM TERM_FROM_ATOM_INDEX(CTS_PIN_ATOM_INDEX)
#define DEFAULT_ATOM TERM_FROM_ATOM_INDEX(DEFAULT_ATOM_INDEX)
#define EVENT_QUEUE_LEN_ATOM TERM_FROM_ATOM_INDEX(EVENT_QUEUE_LEN_ATOM_INDEX)

#endif
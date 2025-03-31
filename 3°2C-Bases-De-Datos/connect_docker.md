1 - Crear el contenedor en docker de mysql con `./start_container.sh`
2 - Verificar con `sudo docker ps`
3 - Si hay un schema para importar hacer `sudo docker cp <schema>.sql <container-name>:/<schema>.sql`
4 - Ejecutar el contenedor con `sudo docker exec -it <container-name> mysql -uroot -p`


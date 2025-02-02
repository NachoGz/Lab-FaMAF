/*
 * fat_fuse_ops.c
 *
 * FAT32 filesystem operations for FUSE (Filesystem in Userspace)
 */

#include "fat_fuse_ops.h"

#include "fat_file.h"
#include "fat_filename_util.h"
#include "fat_fs_tree.h"
#include "fat_util.h"
#include "fat_volume.h"
#include <alloca.h>
#include <errno.h>
#include <gmodule.h>
#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include <stdbool.h>
#include "big_brother.h"

/* Retrieve the currently mounted FAT volume from the FUSE context. */
static inline fat_volume get_fat_volume() {
    return fuse_get_context()->private_data;
}

#define LOG_MESSAGE_SIZE 100
#define DATE_MESSAGE_SIZE 30

void now_to_str(char *buf) {
    time_t now = time(NULL);
    struct tm *timeinfo;
    timeinfo = localtime(&now);

    strftime(buf, DATE_MESSAGE_SIZE, "%d-%m-%Y %H:%M", timeinfo);
}

/* Writes to disk @child_disk_entry, in the position @nentry of the @parent*/
static void write_dir_entry(fat_file parent, fat_dir_entry child_disk_entry,
                            u32 nentry) {
    // Calculate the starting position of the directory
    u32 chunk_size = fat_table_bytes_per_cluster(parent->table);
    off_t parent_offset =
        fat_table_cluster_offset(parent->table, parent->start_cluster);
    size_t entry_size = sizeof(struct fat_dir_entry_s);
    if (chunk_size <= nentry * entry_size) {
        errno = ENOSPC; // TODO we should add a new cluster to the directory.
        DEBUG("The parent directory is full.");
        return;
    }
    DEBUG("Writting dentry on directory %s, entry %u", parent->name, nentry);
    // Calculate the position of the next entry
    off_t entry_offset = (off_t)(nentry * entry_size) + parent_offset;
    ssize_t written_bytes =
        pwrite(parent->table->fd, child_disk_entry, entry_size, entry_offset);
    if (written_bytes < entry_size) {
        errno = EIO;
        DEBUG("Error writing child disk entry");
    }
}

// TODO: complete this function to log to file
void fat_fuse_log_activity(char *operation_type, fat_file target_file)
{
    char buf[LOG_MESSAGE_SIZE] = "";
    fat_volume vol;
    fat_tree_node file_node;
    fat_file file;
    fat_file parent;
    errno = 0;

    now_to_str(buf);
    strcat(buf, "\t");
    strcat(buf, getlogin());
    strcat(buf, "\t");
    strcat(buf, target_file->filepath);
    strcat(buf, "\t");
    strcat(buf, operation_type);
    strcat(buf, "\n");

    vol = get_fat_volume();
    // lo busco en el file tree
    file_node = fat_tree_node_search(vol->file_tree, LOG_FILE);

    parent = fat_tree_get_parent(file_node);
    file = fat_tree_get_file(file_node);
    
    fat_file_pwrite(file, buf, strlen(buf), file->dentry->file_size, parent);
}

/* Get file attributes (file descriptor version) */
int fat_fuse_fgetattr(const char *path, struct stat *stbuf,
                      struct fuse_file_info *fi) {
    fat_file file = (fat_file)fat_tree_get_file((fat_tree_node)fi->fh);
    fat_file_to_stbuf(file, stbuf);
    return 0;
}

/* Get file attributes (path version) */
int fat_fuse_getattr(const char *path, struct stat *stbuf) {
    fat_volume vol;
    fat_file file;

    vol = get_fat_volume();
    file = fat_tree_search(vol->file_tree, path);
    if (file == NULL) {
        errno = ENOENT;
        return -errno;
    }
    fat_file_to_stbuf(file, stbuf);

    return 0;
}

/* Open a file */
int fat_fuse_open(const char *path, struct fuse_file_info *fi) {
    fat_volume vol;
    fat_tree_node file_node;
    fat_file file;

    vol = get_fat_volume();
    file_node = fat_tree_node_search(vol->file_tree, path);
    if (!file_node)
        return -errno;
    file = fat_tree_get_file(file_node);
    if (fat_file_is_directory(file))
        return -EISDIR;
    fat_tree_inc_num_times_opened(file_node);
    fi->fh = (uintptr_t)file_node;
    return 0;
}

/* Open a directory */
int fat_fuse_opendir(const char *path, struct fuse_file_info *fi) {
    fat_volume vol = NULL;
    fat_tree_node file_node = NULL;
    fat_file file = NULL;

    vol = get_fat_volume();
    file_node = fat_tree_node_search(vol->file_tree, path);
    if (file_node == NULL) {
        return -errno;
    }
    file = fat_tree_get_file(file_node);
    if (!fat_file_is_directory(file)) {
        return -ENOTDIR;
    }
    fat_tree_inc_num_times_opened(file_node);
    fi->fh = (uintptr_t)file_node;
    return 0;
}

/* Read directory children. Calls function fat_file_read_children which returns
 * a list of files inside a GList. The children were read from the directory
 * entries in the cluster of the directory.
 * This function iterates over the list of children and adds them to the
 * file tree.
 * This operation should be performed only once per directory, the first time
 * readdir is called.
 */
static void fat_fuse_read_children(fat_tree_node dir_node) {
    fat_volume vol = get_fat_volume();
    fat_file dir = fat_tree_get_file(dir_node);
    GList *children_list = fat_file_read_children(dir);
    // Add child to tree. TODO handle duplicates
    for (GList *l = children_list; l != NULL; l = l->next) {
        vol->file_tree =
            fat_tree_insert(vol->file_tree, dir_node, (fat_file)l->data);
    }
    fat_tree_print_preorder(vol->file_tree);
}

/* Add entries of a directory in @fi to @buf using @filler function. */ 
int fat_fuse_readdir(const char *path, void *buf, fuse_fill_dir_t filler,
                     off_t offset, struct fuse_file_info *fi) {
    errno = 0;
    fat_tree_node dir_node = (fat_tree_node)fi->fh;
    fat_file dir = fat_tree_get_file(dir_node);
    fat_file *children = NULL, *child = NULL;
    int error = 0;

    // Insert first two filenames (. and ..)
    if ((*filler)(buf, ".", NULL, 0) || (*filler)(buf, "..", NULL, 0)) {
        return -errno;
    }
    if (!fat_file_is_directory(dir)) {
        errno = ENOTDIR;
        return -errno;
    }
    if (dir->children_read != 1) {
        fat_fuse_read_children(dir_node);
        if (errno < 0) {
            return -errno;
        }
    }
    
    children = fat_tree_flatten_h_children(dir_node);
    child = children;
    while (*child != NULL) {
        // si el archivo es igual al LOGFILE , no lo muestro
        if (!is_log_filepath((*child)->filepath)) {
            error = (*filler)(buf, (*child)->name, NULL, 0);
            if (error != 0) {
                return -errno;
            }    
        }
        child++;
    }
    
    fat_file logfile;
    fat_volume vol = get_fat_volume();

    fat_tree_node file_node = fat_tree_node_search(vol->file_tree, LOG_FILE);
    // si no existe lo creo y logueo la operacion INIT
    if (file_node == NULL) { // || errno != 0
        printf("%s no existía\n", LOG_FILE);
        if (fat_fuse_mknod(LOG_FILE, 0644, 0) < 0) {
            printf("error creating fs.log\n");
        }
        // vuelvo a buscar el file node ahora que fs.log ya está creado
        file_node = fat_tree_node_search(vol->file_tree, LOG_FILE);
        logfile = fat_tree_get_file(file_node);
        logfile->dentry->base_name[0] = 0xE5;
        logfile->dentry->attribs = FILE_ATTRIBUTE_RESERVED;
        write_dir_entry(fat_tree_get_parent(file_node), logfile->dentry, logfile->pos_in_parent);
        fat_fuse_log_activity("INIT", logfile);
    }
    return 0;
}

/* Read data from a file */
int fat_fuse_read(const char *path, char *buf, size_t size, off_t offset,
                  struct fuse_file_info *fi) {
    errno = 0;
    int bytes_read;
    fat_tree_node file_node = (fat_tree_node)fi->fh;
    fat_file file = fat_tree_get_file(file_node);
    fat_file parent = fat_tree_get_parent(file_node);
    bytes_read = fat_file_pread(file, buf, size, offset, parent);
    if (errno != 0) {
        return -errno;
    }
    
    // logueo que hubo una operación de lectura al archivo file
    fat_fuse_log_activity("read", file);
    return bytes_read;
}

/* Write data from a file */
int fat_fuse_write(const char *path, const char *buf, size_t size, off_t offset,
                   struct fuse_file_info *fi) {
    fat_tree_node file_node = (fat_tree_node)fi->fh;
    fat_file file = fat_tree_get_file(file_node);
    fat_file parent = fat_tree_get_parent(file_node);

    if (size == 0)
        return 0; // Nothing to write
    if (offset > file->dentry->file_size)
        return -EOVERFLOW;
    
    
    // logueo que hubo una operación de escritura al archivo parent
    fat_fuse_log_activity("write", file);
    return fat_file_pwrite(file, buf, size, offset, parent);
}

/* Close a file */
int fat_fuse_release(const char *path, struct fuse_file_info *fi) {
    fat_tree_node file = (fat_tree_node)fi->fh;
    fat_tree_dec_num_times_opened(file);
    return 0;
}

/* Close a directory */
int fat_fuse_releasedir(const char *path, struct fuse_file_info *fi) {
    fat_tree_node file = (fat_tree_node)fi->fh;
    fat_tree_dec_num_times_opened(file);
    return 0;
}

int fat_fuse_mkdir(const char *path, mode_t mode) {
    errno = 0;
    fat_volume vol = NULL;
    fat_file parent = NULL, new_file = NULL;
    fat_tree_node parent_node = NULL;

    // The system has already checked the path does not exist. We get the parent
    vol = get_fat_volume();
    parent_node = fat_tree_node_search(vol->file_tree, dirname(strdup(path)));
    if (parent_node == NULL) {
        errno = ENOENT;
        return -errno;
    }
    parent = fat_tree_get_file(parent_node);
    if (!fat_file_is_directory(parent)) {
        fat_error("Error! Parent is not directory\n");
        errno = ENOTDIR;
        return -errno;
    }

    // init child
    new_file = fat_file_init(vol->table, true, strdup(path));
    if (errno != 0) {
        return -errno;
    }
    // insert to directory tree representation
    vol->file_tree = fat_tree_insert(vol->file_tree, parent_node, new_file);
    // write file in parent's entry (disk)
    fat_file_dentry_add_child(parent, new_file);
    return -errno;
}

/* Creates a new file in @path. @mode and @dev are ignored. */
int fat_fuse_mknod(const char *path, mode_t mode, dev_t dev) {
    errno = 0;
    fat_volume vol;
    fat_file parent, new_file;
    fat_tree_node parent_node;

    // The system has already checked the path does not exist. We get the parent
    vol = get_fat_volume();
    parent_node = fat_tree_node_search(vol->file_tree, dirname(strdup(path)));
    if (parent_node == NULL) {
        errno = ENOENT;
        return -errno;
    }
    parent = fat_tree_get_file(parent_node);
    if (!fat_file_is_directory(parent)) {
        fat_error("Error! Parent is not directory\n");
        errno = ENOTDIR;
        return -errno;
    }
    new_file = fat_file_init(vol->table, false, strdup(path));

    if (errno < 0) {
        return -errno;
    }

    // insert to directory tree representation
    vol->file_tree = fat_tree_insert(vol->file_tree, parent_node, new_file);
    // Write dentry in parent cluster
    fat_file_dentry_add_child(parent, new_file);
    return -errno;
}

int fat_fuse_utime(const char *path, struct utimbuf *buf) {
    errno = 0;
    fat_file parent = NULL;
    fat_volume vol = get_fat_volume();
    fat_tree_node file_node = fat_tree_node_search(vol->file_tree, path);
    if (file_node == NULL || errno != 0) {
        errno = ENOENT;
        return -errno;
    }
    parent = fat_tree_get_parent(file_node);
    if (parent == NULL || errno != 0) {
        DEBUG("WARNING: Setting time for parent ignored");
        return 0; // We do nothing, no utime for parent
    }
    fat_utime(fat_tree_get_file(file_node), parent, buf);
    return -errno;
}

/* Shortens the file at the given offset.*/
int fat_fuse_truncate(const char *path, off_t offset) {
    errno = 0;
    fat_volume vol = get_fat_volume();
    fat_file file = NULL, parent = NULL;
    fat_tree_node file_node = fat_tree_node_search(vol->file_tree, path);
    if (file_node == NULL || errno != 0) {
        errno = ENOENT;
        return -errno;
    }
    file = fat_tree_get_file(file_node);
    if (fat_file_is_directory(file))
        return -EISDIR;

    parent = fat_tree_get_parent(file_node);
    fat_tree_inc_num_times_opened(file_node);
    fat_file_truncate(file, offset, parent);
    return -errno;
}


int fat_fuse_unlink(const char *path) {
    fat_volume vol = NULL;
    fat_tree_node file_node;
    fat_file file, parent;
    u32 curr_cluster, next_cluster;
    errno = 0;
    vol = get_fat_volume();

    // lo busco en el file tree
    file_node = fat_tree_node_search(vol->file_tree, path);

    // chequeo que exista
    if (file_node == NULL || errno != 0) { 
        errno = ENOENT;
        return -errno;
    }

    file = fat_tree_get_file(file_node);
    
    parent = fat_tree_get_parent(file_node);

    // chequeo que no sea un directorio
    if (fat_file_is_directory(file)) {
        errno = EISDIR;
        return -errno;
    }
    
    curr_cluster = file->start_cluster;

    // marco como libres los cluster correspondientes
    // asegurarme que todos los clusters se esten eliminando !!
    while (!fat_table_is_EOC(file->table, curr_cluster)) 
    {
        next_cluster = fat_table_get_next_cluster(file->table, curr_cluster);
        fat_table_set_next_cluster(file->table, curr_cluster, FAT_CLUSTER_FREE);
        fat_file_empty_cluster(file->table, curr_cluster);
        if (errno == EIO) {
            printf("I/O error");
            return -errno;
        }
        curr_cluster = next_cluster;
    }

    // Update entrance in directory
    file->dentry->base_name[0] = 0xE5;
    
    // escribo a disco los cambios
    write_dir_entry(parent, file->dentry, file->pos_in_parent);

    // actualizo el arbol de directorios
    vol->file_tree = fat_tree_delete(vol->file_tree, path);
    
    return 0;

    
}

/**
 * 1. Marcar como libres los clusters correspondientes (incluido el primero)
 * 2. Actualizar la entrada de directorio correspondiente (y acordarse de guardar este cambio a disco!)
 * 3. Actualizar el árbol de directorios en memoria.
*/
/* Elimina un directorio vacío*/

int fat_fuse_rmdir(const char *path) {
    fat_volume vol = NULL;
    fat_tree_node dir_node;
    fat_file dir, parent;
    u32 curr_cluster, next_cluster;
    errno = 0;
    vol = get_fat_volume();
    // lo busco en el file tree
    dir_node = fat_tree_node_search(vol->file_tree, path);
    // chequeo que exista
    if (dir_node == NULL || errno != 0) { 
        errno = ENOENT;
        return -errno;
    }
    
    parent = fat_tree_get_parent(dir_node);
    dir = fat_tree_get_file(dir_node);
    // chequeo que sea un directorio
    if (!fat_file_is_directory(dir)) {
        errno = ENOTDIR;
        return -errno;
    }
    // chequeo que este vacio
    GList *children_list = fat_file_read_children(dir);
    if (g_list_length(children_list) != 0) {
        errno = ENOTEMPTY;
        return -errno;
    }

    curr_cluster = dir->start_cluster;

    // marco como libres los cluster correspondientes
    while (!fat_table_is_EOC(dir->table, curr_cluster)) 
    {
        next_cluster = fat_table_get_next_cluster(dir->table, curr_cluster);
        fat_table_set_next_cluster(dir->table, curr_cluster, FAT_CLUSTER_FREE);
        fat_file_empty_cluster(dir->table, curr_cluster);
        curr_cluster = next_cluster;
        if (errno == EIO) {
            printf("I/O error");
            return 0;
        }
    }

    // lo marco como 'listo para eliminar'
    dir->dentry->base_name[0] = 0xE5;

    // escribo a disco los cambios
    write_dir_entry(parent, dir->dentry, dir->pos_in_parent);

    // lo elimino del arbol de directorios
    vol->file_tree = fat_tree_delete(vol->file_tree, path);

    return 0;
}
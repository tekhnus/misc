package main

import (
	"errors"
	"flag"
	"fmt"
	"github.com/pelletier/go-toml"
	"io/fs"
	"log"
	"os"
	"path/filepath"
	"strings"
	"time"
)

type CatalogConfig struct {
	Path string
}
type CatalogsConfig struct {
	Catalogs []CatalogConfig
}

func main() {
	addCatalogCmd := flag.NewFlagSet("add-catalog", flag.ExitOnError)
	installCmd := flag.NewFlagSet("install", flag.ExitOnError)

	flag.Parse()
	command := flag.Arg(0)
	commandArgs := flag.Args()[1:]

	switch command {
	case "add-catalog":
		addCatalogCmd.Parse(commandArgs)
		catalog := addCatalogCmd.Arg(0)
		err := addCatalog(catalog)
		if err != nil {
			log.Fatal(err)
		}
	case "install":
		installCmd.Parse(commandArgs)
		packages := installCmd.Args()
		for _, pkg := range packages {
			err := install(pkg)
			if err != nil {
				log.Fatal(err)
			}
		}
	default:
		fmt.Println("unknown command")
		os.Exit(1)
	}
}

func defaultCatalogConfig() (string, error) {
	homedir, err := os.UserHomeDir()
	if err != nil {
		return "", err
	}
	path := filepath.Join(homedir, ".config/dotfile-manager/catalogs.toml")
	return path, nil
}

func openCatalogConfig() (*os.File, error) {
	path, err := defaultCatalogConfig()
	if err != nil {
		return nil, err
	}
	f, err := os.Open(path)
	if err != nil && errors.Is(err, os.ErrNotExist) {
		return nil, nil
	}
	return f, err
}

func writableCatalogConfig() (*os.File, error) {
	path, err := defaultCatalogConfig()
	if err != nil {
		return nil, err
	}
	f, err := openCatalogConfig()
	if err != nil {
		return nil, err
	}
	if f != nil {
		f.Close()
		path = f.Name()
	}
	return os.Create(path)
}

func getCatalogConfig() (CatalogsConfig, error) {
	var cfg CatalogsConfig
	f, err := openCatalogConfig()
	if err != nil {
		return cfg, err
	}
	if f == nil {
		cfg = CatalogsConfig{}
		res, _ := toml.Load("catalogs = []")
		res.Unmarshal(&cfg)
		return cfg, nil
	}
	defer f.Close()
	catalogsconf, err := toml.LoadReader(f)
	if err != nil {
		return cfg, err
	}
	catalogsconf.Unmarshal(&cfg)
	return cfg, nil
}

func walkPkg(pkgname string, walkfunc func(string, string, os.FileInfo, error) error) error {
	catalogs, err3 := getCatalogConfig()
	if err3 != nil {
		return err3
	}
	var catpath, pkgpath string

	var err error = nil
	found := false
	for _, catconf := range catalogs.Catalogs {
		catpath = catconf.Path

		pkgpath = filepath.Join(catpath, pkgname)
		exerr := walkPkgByPath(pkgpath, exfunc)
		if exerr != nil {
			continue
		}
		err = walkPkgByPath(pkgpath, walkfunc)
		if err != nil {
			break
		}
		found = true
	}
	if !found && err == nil {
		err = errors.New("package " + pkgname + " not found")
	}
	return err
}

func exfunc(x string, y string, i os.FileInfo, e error) error {
	if e != nil {
		return e
	}
	return fs.SkipDir
}

func walkPkgByPath(pkgpath string, walkfunc func(string, string, os.FileInfo, error) error) error {
	homedir, err1 := os.UserHomeDir()
	if err1 != nil {
		return err1
	}
	destdir := homedir
	return filepath.Walk(pkgpath, func(path string, info os.FileInfo, err error) error {
		if err == nil && info.IsDir() && info.Name() == ".git" {
			return fs.SkipDir
		}
		relpath, relerr := filepath.Rel(pkgpath, path)
		if relerr != nil {
			return relerr
		}
		destpath := filepath.Join(destdir, relpath)
		return walkfunc(path, destpath, info, err)
	})
}

func prettify(path string) string {
	homedir, err := os.UserHomeDir()
	if err != nil {
		return path
	}
	reltohome, err := filepath.Rel(homedir, path)
	if err != nil {
		return path
	}
	return filepath.Join("~", reltohome)
}

func moveToBackup(path string) error {
	homedir, err := os.UserHomeDir()
	if err != nil {
		return err
	}
	suffix := time.Now().Format(time.RFC3339)
	backupname := strings.Join(strings.Split(path, "/"), "!") + "#" + suffix
	backupdir := filepath.Join(homedir, ".cache/dotfile-manager/backup")
	err = os.MkdirAll(backupdir, os.ModeDir|0700)
	if err != nil {
		return err
	}
	backupfile := filepath.Join(backupdir, backupname)
	err = os.Rename(path, backupfile)
	if err == nil {
		fmt.Println("Moved", prettify(path), "to backup")
	}
	return err
}

func installFile(srcpath string, destpath string) error {
	err := os.Symlink(srcpath, destpath)
	if err == nil {
		fmt.Println("Symlinked", prettify(destpath))
		return nil
	}
	if !errors.Is(err, os.ErrExist) {
		return err
	}
	referencedDestpath, err := os.Readlink(destpath)
	if err == nil && referencedDestpath == srcpath {
		fmt.Println("Already up-to-date:", prettify(destpath))
		return nil
	}
	err = moveToBackup(destpath)
	if err != nil {
		return err
	}
	err = os.Symlink(srcpath, destpath)
	if err == nil {
		fmt.Println("Symlinked", prettify(destpath))
	}
	return err
}

func installDir(destpath string) error {
	err := os.Mkdir(destpath, os.ModeDir|0700)
	if err == nil {
		fmt.Println("Created", destpath)
		return nil
	}
	stat, err := os.Stat(destpath)
	if err != nil || !stat.IsDir() {
		return errors.New("Could not create a directory: " + destpath)
	}
	return nil
}

func installEntry(path string, destpath string, info os.FileInfo, err error) error {
	if err != nil {
		return err
	}
	if info.IsDir() {
		return installDir(destpath)
	}
	return installFile(path, destpath)
}

func install(pkg string) error {
	return walkPkg(pkg, installEntry)
}

func addCatalog(catpath string) error {
	catalogs, err := getCatalogConfig()
	if err != nil {
		return err
	}

	catpath, err3 := filepath.Abs(catpath)
	if err3 != nil {
		return err3
	}
	_, err2 := os.Lstat(filepath.Join(catpath, "manifest.toml"))
	if err2 != nil {
		return err2
	}

	addendum := CatalogConfig{Path: catpath}
	catlist := catalogs.Catalogs
	catalogs.Catalogs = append(catlist, addendum)

	f, err := writableCatalogConfig()
	if err != nil {
		return err
	}
	defer f.Close()

	b, err := toml.Marshal(catalogs)
	if err != nil {
		return err
	}
	_, err = f.Write(b)
	return err
}

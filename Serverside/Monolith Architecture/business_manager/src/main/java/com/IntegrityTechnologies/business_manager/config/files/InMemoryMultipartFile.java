package com.IntegrityTechnologies.business_manager.config.files;

import org.springframework.web.multipart.MultipartFile;

import java.io.*;
import java.nio.file.Files;

public class InMemoryMultipartFile implements MultipartFile {

    private final String fileName;
    private final byte[] content;

    public InMemoryMultipartFile(String fileName, byte[] content) {
        this.fileName = fileName;
        this.content = content;
    }

    @Override
    public String getName() {
        return fileName;
    }

    @Override
    public String getOriginalFilename() {
        return fileName;
    }

    @Override
    public String getContentType() {
        return null;
    }

    @Override
    public boolean isEmpty() {
        return content == null || content.length == 0;
    }

    @Override
    public long getSize() {
        return content.length;
    }

    @Override
    public byte[] getBytes() {
        return content;
    }

    @Override
    public InputStream getInputStream() {
        return new ByteArrayInputStream(content);
    }

    @Override
    public void transferTo(File dest) throws IOException {
        Files.write(dest.toPath(), content);
    }
}
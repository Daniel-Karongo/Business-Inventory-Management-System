package com.IntegrityTechnologies.business_manager.common;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.util.JwtUtil;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

;

@SpringBootTest
@AutoConfigureMockMvc
@TestInstance(TestInstance.Lifecycle.PER_CLASS) // Allows @BeforeAll to use @Autowired
public abstract class BaseIntegrationTest {

    @Autowired protected MockMvc mvc;
    @Autowired protected JwtUtil jwtUtil;
    @Autowired protected UserRepository userRepository;
    @Autowired protected ObjectMapper om;

    protected String token;

    @BeforeAll
    void generateJwt() {
        // Ensure test user exists
        User user = userRepository.findByUsername("Daniel Karongo").get();

        // Generate JWT for this test user
        token = "Bearer " + jwtUtil.generateToken(user.getId(), user.getUsername(), user.getRole().toString());
    }

    /** Helper for authenticated GET */
    protected org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder authGet(String url) {
        return org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get(url)
                .header("Authorization", token)
                .contentType(MediaType.APPLICATION_JSON);
    }

    /** Helper for authenticated POST */
    protected org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder authPost(String url, String body) {
        return org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post(url)
                .header("Authorization", token)
                .contentType(MediaType.APPLICATION_JSON)
                .content(body);
    }

    /** Helper for authenticated PUT */
    protected org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder authPut(String url, String body) {
        return org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put(url)
                .header("Authorization", token)
                .contentType(MediaType.APPLICATION_JSON)
                .content(body);
    }

    /** Helper for authenticated DELETE */
    protected org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder authDelete(String url) {
        return org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete(url)
                .header("Authorization", token)
                .contentType(MediaType.APPLICATION_JSON);
    }
}
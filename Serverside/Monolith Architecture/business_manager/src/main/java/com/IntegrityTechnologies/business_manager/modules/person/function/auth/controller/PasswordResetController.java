package com.IntegrityTechnologies.business_manager.modules.person.function.auth.controller;

import com.IntegrityTechnologies.business_manager.modules.person.function.auth.config.PasswordResetProperties;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.model.PasswordResetToken;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.service.PasswordResetEmailService;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.service.PasswordResetService;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.service.SmsService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.service.PasswordResetSmsService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequestMapping("/api/auth/password-reset")
@RequiredArgsConstructor
public class PasswordResetController {

    private final PasswordResetProperties props;
    private final PasswordResetService service;
    private final UserRepository userRepository;
    private final PasswordResetEmailService emailService;
    private final PasswordResetSmsService smsResetService;
    private final SmsService smsService;

    @GetMapping("/options")
    public ResponseEntity<?> options() {
        return ResponseEntity.ok(Map.of(
                "channels", props.getChannels(),
                "identityFields", props.getIdentity().getRequiredFields()
        ));
    }

    @PostMapping("/initiate")
    public ResponseEntity<?> initiate(@RequestBody Map<String, String> req) {

        String identifier = req.get("identifier");
        String channel = req.get("channel");

        // Validate channel is allowed by configuration
        PasswordResetProperties.Channel selectedChannel;
        try {
            selectedChannel = PasswordResetProperties.Channel.valueOf(channel);
        } catch (Exception e) {
            return ResponseEntity.badRequest().body("Invalid channel");
        }

        if (!props.getChannels().contains(selectedChannel)) {
            return ResponseEntity.badRequest().body("Channel not allowed");
        }

        userRepository.findByIdentifier(identifier).ifPresent(user -> {

            switch (selectedChannel) {

                case EMAIL -> {
                    if (!props.getEmail().isEnabled()) return;

                    var token = service.createToken(
                            user.getId(),
                            PasswordResetToken.Channel.EMAIL,
                            props.getEmail().getTokenExpiryMinutes()
                    );
                    emailService.sendResetEmail(user, token.getTokenHash());
                }

                case SMS -> {
                    if (!props.getSms().isEnabled()) return;

                    var token = service.createToken(
                            user.getId(),
                            PasswordResetToken.Channel.SMS,
                            props.getSms().getTokenExpiryMinutes()
                    );
                    smsResetService.sendResetSms(user, token.getTokenHash());
                }

                case IDENTITY -> {
                    // No messaging, handled during /complete
                }
            }
        });

        // Always OK (anti-account enumeration)
        return ResponseEntity.ok().build();
    }

    @PostMapping("/verify")
    public ResponseEntity<?> verify(@RequestBody Map<String, String> req) {

        String rawToken = req.get("token");

        service.verifyToken(rawToken);

        return ResponseEntity.ok().build();
    }

    @PostMapping("/complete")
    public ResponseEntity<?> complete(@RequestBody Map<String, String> req) {

        if (req.containsKey("token")) {
            service.resetWithToken(req.get("token"), req.get("newPassword"));
        } else {
            service.resetWithIdentity(
                    req.get("identifier"),
                    req.get("idNumber"),
                    req.get("email"),
                    req.get("phone"),
                    req.get("newPassword")
            );
        }

        return ResponseEntity.ok().build();
    }
}